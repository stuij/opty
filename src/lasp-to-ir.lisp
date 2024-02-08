(in-package :opty)

;; environment
(defclass var ()
  ((name     :initarg :name     :accessor name)
   (env-id   :initarg :env-id   :accessor env-id)
   (temp     :initarg :temp     :accessor temp)
   (var-type :initarg :var-type :accessor var-type :initform nil)))

(defclass env ()
  ((id          :initarg :id          :accessor id          :initform 0)
   (parent      :initarg :parent      :accessor parent      :initform nil)
   (children    :initarg :children    :accessor children    :initform '())
   (vars        :initarg :vars        :accessor vars        :initform (make-hash-table))
   (env-counter :initarg :env-counter :accessor env-counter :initform (counter))))

(defmethod expand-env ((parent env))
  (with-slots (env-counter) parent
    (let ((child (make-instance 'env :id (funcall env-counter)
                                     :parent parent
                                     :env-counter env-counter)))
      (setf (children parent)
            (append (children parent)
                    (list child)))
      child)))

(defun lookup (var env)
  (if (gethash var (vars env))
      (gethash var (vars env))
      (if (parent env)
          (lookup var (parent env)))))

(defun make-env ()
  (make-instance 'env))

(defun to-env (var temp env &optional type)
  (assert (not (lookup var env)) (var) "Var ~S is already present in env." var)
  (let ((instance (make-instance 'var
                            :name var
                            :env-id (id env)
                            :temp temp
                            :var-type type)))
    (setf (gethash var (vars env)) instance)))




;; Mapping of source language to opcode name for basic ops.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *builtins*
    (make-hash-table)))

(defmacro plain-op-to-builtins (op-list)
  `(progn
     ,@(loop for op-spec in op-list
             collect  (let ((src-op (car op-spec))
                            (ir-op (cadr op-spec)))
                        `(setf (gethash ',src-op *builtins*)
                               (lambda (args expr env graph)
                                 (plain-op-to-flow
                                  ',ir-op args expr env graph)))))))


(plain-op-to-builtins
 ((add iadd)
  (mul imul)
  (<   ilt)))


;; parse source language to IR
(defun plain-op-to-flow (op args expr env graph)
  (let* ((args (loop for expr in args
                     collect (to-flow expr env graph))))
    (emit-op op args graph expr)))

(defun if-to-flow (args expr env graph)
  (assert (<= (length args) 3) (args)
          "argument arity to `if` exceeds 3: ~A" args)
  (assert (> (length args) 1) (args)
          "`if` needs at least a condition and a clause: ~A" args)
  (destructuring-bind (cond-form true-form false-form) args
      (let* ((bb-true (create-bb graph))
             (bb-false (create-bb graph))
             (bb-cont (create-bb graph))
             (cond-temp (to-flow cond-form env graph))
             (ret (new-temp (temp-table graph))))
        (emit-op 'ibcond (list cond-temp bb-true bb-false) graph expr)
        (start-block bb-true graph)
        (let ((tmp-true (to-flow true-form env graph)))
          (emit-op 'icpy (list ret tmp-true) graph expr)
          (emit-op 'jmp (list bb-cont) graph expr))
        (start-block bb-false graph)
        (let ((tmp-false (to-flow false-form env graph)))
          (emit-op 'icpy (list ret tmp-false) graph expr)
          (emit-op 'jmp (list bb-cont) graph expr))
        (start-block bb-cont graph)
        ret)))

(setf (gethash 'if *builtins*)  #'if-to-flow)

;; Very similar to if expression, but as we don't need to evaluate a body.
;; I hope we can get away with not branching to a new bb for the 1st clause,
;; and optimizing out the double ret assignment to a phi node later on.
(defun and-to-flow (args expr env graph)
  (assert (= (length args) 2) (args)
          "argument arity to `and` isn't 2: ~A" args)
  (let* ((bb-2nd-clause (create-bb graph))
         (bb-cont (create-bb graph))
         ;; code to calc 1st arg is handled in the current block
         (1st-clause-tmp (to-flow (car args) env graph))
         (ret (new-temp (temp-table graph) (temp-type 1st-clause-tmp))))
    (emit-op 'icpy (list ret 1st-clause-tmp) graph expr )
    (emit-op 'ibcond (list 1st-clause-tmp bb-2nd-clause bb-cont) graph expr)
    (start-block bb-2nd-clause graph)
    (let ((2nd-clause-ret (to-flow (cadr args) env graph)))
      (emit-op 'icpy (list ret 2nd-clause-ret) graph expr)
      (emit-op 'jmp (list bb-cont) graph expr))
    (start-block bb-cont graph)
    ret))

(setf (gethash 'and *builtins*)  #'and-to-flow)

(defun to-flow (expr env graph)
  (if (atom expr)
      (if-let (var (lookup expr env))
        (temp var)
        (error "Var not found in environment: ~A" expr))
      (let ((thing (car expr)))
        (if-let (handler (gethash thing *builtins*))
          (funcall handler (cdr expr) expr env graph)
          ;; TODO: check function environment
          (error "Op not supported yet: ~S" thing)))))

(defun handle-args (args graph env)
  (assert (= (length args) (length (remove-duplicates args)))
            (args) "Function argument list contains duplicates: ~A" args)
  (loop for a in args
        do (progn
             (assert (symbolp a) (a)
                     "Function arguments should only consist of symbols: ~A" a)
             (let ((temp (to-temp a
                                  (temp-table graph)
                                  :source a
                                  :name a
                                  :first t)))
               (setf (args graph)
                     (append (args graph)
                             (list temp)))
               (to-env a temp env))))
  args)

(defun emit-ret (ret-temp graph)
  (multiple-value-bind (_ op) (emit-op 'ret (list ret-temp) graph)
    (declare (ignore _))
    (setf (res-types op) (list (temp-type ret-temp)))
    op))

(defun defun-to-flow (expr env)
  (let ((graph (initialize-graph (string-downcase (string (car expr)))))
        (env (expand-env env))
        (args (cadr expr))
        (body (cddr expr)))
    (handle-args args graph env)
    (let ((ret-temp
            (if body
                (loop with ret-temp = nil
                      for e in body
                      do (setf ret-temp (to-flow e env graph))
                      finally (return ret-temp))
                (to-temp nil (temp-table graph) :type 'void))))
      (emit-ret ret-temp graph))
    (setf (exit graph) (current graph))
    graph))

(defun top-to-flow (expr env)
  "top level exprs will return flow-graphs but won't pass them"
  (case (car expr)
    ('defun (defun-to-flow (cdr expr) env))))

(defun exprs-to-flowgraph (exprs)
  "abstract from froms input (file, string, etc..)"
  (let ((env (make-env)))
    (cons
     (loop for f in exprs
	       collect (if (eq (car f) 'defun)
		               (top-to-flow f env)
		               (error "only defun toplevel exprs supported at the moment")))
     env)))

(defun read-exprs-from-stream (stream)
  "`read` stream, so source code gets transexpred into lists of tokens"
  (loop for statement = (read stream nil)
        while statement
        collect statement))

(defun file-to-flow-graph (file)
  (let* ((exprs (with-open-file (stream file)
                  (read-exprs-from-stream stream))))
    (exprs-to-flowgraph exprs)))

(defun to-ir (name)
  "Convenience function to test files from the Lasp dir."
  (file-to-flow-graph (opty-path (format nil "snippets/lasp/~A.lasp" name))))

;; (to-ir "maxcol")
;; (to-ir "simple-fns")
;; (serialize-ir-to-string (car (to-ir "simple-fns")))
#- (and) (with-input-from-string (s (serialize-ir-to-string (car (to-ir "simple-fns"))))
           (read-exprs-from-stream s))

