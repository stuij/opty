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


;; parse source language to IR

;; Mapping of source language to opcode name for basic ops.
(defparameter *src-to-plain-ops-table*
  '((add . iadd) (mul . imul)))

(defun plain-op-to-flow (op args expr env graph)
  (let* ((args (loop for expr in args
                     collect (to-flow expr env graph)))
         (op-instance (funcall (make-op-name op) args))
         (op-ident (op-to-ident op args)))
    (let ((ret (to-temp op-ident (temp-table graph)
                        :source expr
                        :type (op-result-type op-instance))))
      (setf (result op-instance) ret)
      (append-op op-instance graph)
      ret)))

(defun to-flow (expr env graph)
  (if (atom expr)
      (if-let (var (lookup expr env))
        (temp var)
        (error "Var not found in environment: ~A" expr))
      (let ((thing (car expr)))
        (if-let (op-entry (assoc thing *src-to-plain-ops-table*))
          (plain-op-to-flow (cdr op-entry) (cdr expr) expr env graph)
          (error "op not supported yet: ~S" thing)))))

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

(defun defun-to-flow (expr env)
  (let ((graph (initialize-graph (string-downcase (string (car expr)))))
        (env (expand-env env))
        (args (cadr expr))
        (body (cddr expr)))
    (handle-args args graph env)
    (let ((ret (loop with ret = nil
                     for e in body
                     do (setf ret (to-flow e env graph))
                     finally (return ret))))
      (classify-graph graph)
      graph)))

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

