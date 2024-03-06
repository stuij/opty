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

(defun lookup (var env &key (lookup-parents t))
  (if (gethash var (vars env))
      (gethash var (vars env))
      (if (and lookup-parents (parent env))
          (lookup var (parent env) :lookup-parents t))))

(defun make-env ()
  (make-instance 'env))

(defun to-env (var temp env &key type (lookup-parents t))
  (assert (not (lookup var env :lookup-parents lookup-parents)) (var)
          "Var ~S is already present in env." var)
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
                                 (plain-op-to-ir
                                  ',ir-op args expr env graph)))))))

(plain-op-to-builtins
 ((+ add)
  (* mul)
  (< lt)
  (<= le)
  (> gt)
  (>= ge)))


;; parse source language to IR
(defun plain-op-to-ir (op args expr env graph)
  (let* ((args (loop for expr in args
                     collect (to-ir expr env graph))))
    (emit-op op args graph :source expr)))

(defun set-bb-relations (predecessor &rest successors)
  (setf (successors predecessor) (append (successors predecessor)
                                         successors))
  (loop for s in successors
        do (setf (predecessors s) (append (predecessors s)
                                          (list predecessor)))))

(defun emit-bcond (cond-temp bb-true bb-false expr graph)
  (emit-op 'bcond (list cond-temp bb-true bb-false) graph :source expr)
  (set-bb-relations (current graph) bb-true bb-false))

(defun emit-jmp (target expr graph)
  (emit-op 'jmp (list target) graph :source expr)
  (set-bb-relations (current graph) target))

(defun if-to-ir (args expr env graph)
  (assert (<= (length args) 3) (args)
          "argument arity to `if` exceeds 3: ~A" args)
  (assert (> (length args) 1) (args)
          "`if` needs at least a condition and a clause: ~A" args)
  (let* ((cond-form (car args))
         (true-form (cadr args))
         (false-form (caddr args))
         (bb-true (create-bb graph))
         (bb-false (create-bb graph))
         (bb-cont (create-bb graph))
         (cond-temp (to-ir cond-form env graph)))
    (emit-bcond cond-temp bb-true bb-false expr graph)
    (start-block bb-true graph)
    (let* ((tmp-true (to-ir true-form env graph))
           (tmp-true-type (temp-type tmp-true))
           (ret-type (if false-form
                         (temp-type tmp-true)
                         'poison))
           (ret-type-list (list ret-type))
           (ret (new-temp (temp-table graph) :type ret-type)))
      (emit-op 'cpy (list tmp-true) graph
               :source expr
               :operand-types (list tmp-true-type)
               :result-types ret-type-list
               :results (list ret))
      (emit-jmp bb-cont expr graph)
      (start-block bb-false graph)
      (let* ((tmp-false (if false-form
                            (to-ir false-form env graph)
                            (to-ir 'poison env graph)))
             (tmp-false-type (temp-type tmp-false)))
        (assert (eq ret-type tmp-false-type) ()
                "`if` branch return values aren't equal: ~A, ~A"
                ret-type tmp-false-type)
        (emit-op 'cpy (list tmp-false) graph
                 :source expr
                 :operand-types (list tmp-false-type)
                 :result-types ret-type-list
                 :results (list ret)))
      (emit-op 'jmp (list bb-cont) graph :source expr)
      (start-block bb-cont graph)
      ret)))

(setf (gethash 'if *builtins*)  #'if-to-ir)

;; array specs are row-major
;; get array element pointer
(defun aptr-to-ir (args expr env graph)
  (let* ((arr (to-ir (car args) env graph))
         (indices (loop for i in (cdr args)
                        collect (to-ir i env graph)))
         (operand-types (append (list 'ptr)
                                (loop for i in indices
                                      collect (temp-type i))))
         ;; pointer type decays to element type of the array
         (ret-type-info (make-instance 'pointer-info
                                       :pointee-type (arr-type (type-info arr)))))
    (assert (eql (pointee-type (type-info arr)) 'arr) ()
            "The pointee type isn't an array: ~A"
            (pointee-type (type-info arr)))
    (emit-op 'elem (append (list arr) indices) graph
             :source expr
             :operand-types operand-types
             :type-info ret-type-info
             :arity (1+ (length (dimensions (type-info arr)))))))

(setf (gethash 'aptr *builtins*)  #'aptr-to-ir)

;; load array element value
(defun aref-to-ir (args expr env graph)
  (let ((tmp (aptr-to-ir args expr env graph)))
    (emit-op 'ldr (list tmp) graph :source expr)))

(setf (gethash 'aref *builtins*)  #'aref-to-ir)

;; set array element. return the value that the array has been set with
(defun aset-to-ir (args expr env graph)
  (let ((val-tmp (to-ir (car args) env graph))
        (loc-tmp (aptr-to-ir (cdr args) expr env graph)))
    (emit-op 'str (list val-tmp loc-tmp) graph :source expr)
    val-tmp))

(setf (gethash 'aset *builtins*)  #'aset-to-ir)

;; Very similar to if expression, but as we don't need to evaluate a body.
;; I hope we can get away with not branching to a new bb for the 1st clause,
;; and optimizing out the double ret assignment to a phi node later on.
(defun and-to-ir (args expr env graph)
  (assert (= (length args) 2) (args)
          "argument arity to `and` isn't 2: ~A" args)
  (let* ((bb-2nd-clause (create-bb graph))
         (bb-cont (create-bb graph))
         ;; code to calc 1st arg is handled in the current block
         (1st-clause-tmp (to-ir (car args) env graph))
         (ret-type (temp-type 1st-clause-tmp))
         (cpy-type-list (list ret-type))
         (ret (new-temp (temp-table graph) :type ret-type)))
    (emit-op 'cpy (list 1st-clause-tmp) graph
             :source expr
             :operand-types cpy-type-list
             :result-types cpy-type-list
             :results (list ret))
    (emit-bcond 1st-clause-tmp bb-2nd-clause bb-cont expr graph)
    (start-block bb-2nd-clause graph)
    (let ((2nd-clause-tmp (to-ir (cadr args) env graph)))
      (assert (and (eq (temp-type 1st-clause-tmp) 'i32)
                   (eq (temp-type 2nd-clause-tmp) 'i32))
              () "`and` argument values aren't equal to i32: ~A, ~A"
              1st-clause-tmp 2nd-clause-tmp)
      (setf (temp-type ret) 'i32)
      (emit-op 'cpy (list 2nd-clause-tmp) graph
               :source expr
               :operand-types cpy-type-list
               :result-types cpy-type-list
               :results (list ret))
      (emit-op 'jmp (list bb-cont) graph :source expr))
    (start-block bb-cont graph)
    ret))

(setf (gethash 'and *builtins*)  #'and-to-ir)

(defun number-to-ir (nr graph)
  (assert (integerp nr) ()
          "Immediate isn't an integer: ~A" nr)
  (emit-op 'ldi (list nr) graph :source nr))

(defun poison-to-ir (poison graph)
  (emit-op 'ldp '() graph :source poison))

(defun handle-var-form (form env graph)
  (assert (symbolp (car form)) ()
          "Let form variable should be just a symbol: ~A" (car form))
  (let* ((assign-tmp (to-ir (cadr form) env graph))
         (assign-type (temp-type assign-tmp))
         (var-tmp (new-temp (temp-table graph) :type assign-type)))
    (emit-op 'cpy (list assign-tmp) graph
             :source form
             :operand-types (list assign-type)
             :result-types (list assign-type)
             :results (list var-tmp))
    (to-env (car form) var-tmp env :type assign-type :lookup-parents nil)))

(defun handle-var-forms (var-forms env graph)
  (loop for form in var-forms
        do (handle-var-form form env graph)))

(defun body-to-ir (body env graph)
  (loop with ret = nil
        for e in body
        do (setf ret (to-ir e env graph))
        finally (return ret)))

;; Like Common Lisp's let*
(defun let-to-ir (forms expr env graph)
  (declare (ignore expr))
  (let* ((child-env (expand-env env))
         (var-forms (car forms))
         (body (cdr forms)))
    (handle-var-forms var-forms child-env graph)
    (body-to-ir body child-env graph)))

(setf (gethash 'let* *builtins*)  #'let-to-ir)

;; Like Common Lisp's do*.
;; As it's a bit of a complex construction, here's the breakdown:
;; (do ((<var1> <var1-initial-value> <var1-step>)
;;      (<var2> <var2-initial-value> <var2-step>)
;;      ...)
;;     ((<exit-condition>)
;;      (<final-statement1>)
;;      (<final-statement2>)
;;      ...)
;;   (<action1-during-loop>)
;;   (<action2-during-loop>)
;;   ...))
;;
;; This should translate into:
;; - initialize variables and add them to environment <- current block
;; - do an initial check for the exit condition before we enter the loop
;; - enter the loop <- start block
;;   - evaluate the body
;;   - evaluate exit condition
;;   - when exit-condition == t, exit the loop
;;   - execute final statements <- post-loop block
;;
;; the loop should be able to be returned from with an optional form to evaluate:
;; (return ...)
;; TODO: break stack
(defun do*-to-ir (forms expr env graph)
  (declare (ignore expr))
  (let* ((child-env (expand-env env))
         ;; split out forms by function
         (var-specs (car forms))
         (exit-condition (caadr forms))
         (final-forms (cdadr forms))
         (body (cddr forms))
         ;; generate code for initial check and entering the loop
         (bb-loop (create-bb graph))
         (bb-cont (create-bb graph)))
    (multiple-value-bind (var-tmps step-forms)
        ;; we generate code for var initialization, and we need to collect the
        ;; result tmps as they need to be copied into by the step forms,
        ;; which we also collect here
        (loop with var-tmps = '()
              with step-forms = '()
              for spec in var-specs
              do (progn
                   (setf var-tmps
                         (append var-tmps
                                 (list (handle-var-form
                                        (subseq spec 0 2)
                                        child-env graph))))
                   (setf step-forms (append step-forms (list (caddr spec)))))
              finally (return (values var-tmps step-forms)))
      (print var-tmps)
      (let ((exit-tmp (to-ir exit-condition child-env graph)))
        (emit-bcond exit-tmp bb-cont bb-loop exit-condition graph))
      ;; emit body forms
      (start-block bb-loop graph)
      (body-to-ir body child-env graph)
      ;; we now update the variables we collected with the step forms we collected
      (loop for var in var-tmps
            for step in step-forms
            do (if step
                   (let* ((step-tmp (to-ir step child-env graph))
                          (step-type (temp-type step-tmp))
                          (var-tmp (temp var))
                          (var-type (temp-type var-tmp)))
                     (assert (equalp step-type var-type) ()
                             "Var type `~A` and step type `~A` are not the same."
                             step-type var-type)
                     (emit-op 'cpy (list step-tmp) graph
                              :source step
                              :operand-types (list step-type)
                              :result-types (list var-type)
                              :results (list var-tmp)))))
      ;; check for loop exit condition and execute final forms
      (let ((exit-tmp (to-ir exit-condition child-env graph)))
        (emit-bcond exit-tmp bb-cont bb-loop exit-condition graph))
      (start-block bb-cont graph)
      (loop with ret = nil
            for form in final-forms
            do (setf ret (to-ir form child-env graph))
            finally (return ret)))))

(setf (gethash 'do* *builtins*)  #'do*-to-ir)

(defun progn-to-ir (forms expr env graph)
  (declare (ignore expr))
  (body-to-ir forms env graph))

(setf (gethash 'progn *builtins*)  #'progn-to-ir)

(defun to-ir (expr env graph)
  (if (atom expr)
      (cond ((numberp expr)
             (number-to-ir expr graph))
            ((eq expr 'poison)
             (poison-to-ir expr graph))
            (t
             (if-let (var (lookup expr env))
               (temp var)
               (error "Var not found in environment: ~A" expr))))
      (let ((thing (car expr)))
        (if-let (handler (gethash thing *builtins*))
          (funcall handler (cdr expr) expr env graph)
          ;; TODO: check function environment
          (error "Op not supported yet: ~S" thing)))))

(defun get-fn-arg-array-info (type-spec env)
  (let* ((type (cadr type-spec))
         (dimensions
           (loop for i in (caddr type-spec)
                 collect (if (integerp i)
                             i
                             (if-let (val (lookup i env))
                               (temp val)
                               (error "array index isn't a known value or ~
                               integer: ~A" i))))))
    (make-instance 'array-info
                   :arr-type type
                   :dimensions dimensions)))

(defun args-to-ir (args graph env)
  (assert (= (length args) (length (remove-duplicates args)))
          (args) "Function argument list contains duplicates: ~A" args)
  (loop for a in args
        do (progn
             (assert (symbolp (car a)) ((car a))
                     "Function argument name should only consist of symbols: ~A"
                     a)
             (let* ((name (car a))
                    (type-expr (cadr a))
                    (type (if (atom type-expr)
                              type-expr
                              (if (eq (car type-expr) 'arr)
                                  'ptr
                                  (error "Unknown compound function argument: ~A"
                                         type-expr))))
                    (type-info (if (and (listp type-expr) (eq (car type-expr) 'arr))
                                   (get-fn-arg-array-info type-expr env)))
                    (temp (to-temp a
                                   (temp-table graph)
                                   :source a
                                   :first t
                                   :type type
                                   :type-info type-info)))
               (setf (args graph)
                     (append (args graph)
                             (list temp)))
               (to-env name temp env :type type))))
  args)

(defun defun-to-ir (expr env)
  (let ((graph (initialize-graph (string-downcase (string (car expr)))))
        (env (expand-env env))
        (ret-val (cadr expr))
        (args (caddr expr))
        (body (cdddr expr)))
    (args-to-ir args graph env)
    (let ((ret-temp
            (if body
                (if-let (ret (body-to-ir body env graph))
                  ret
                  (to-temp nil (temp-table graph) :type 'void))
                (to-temp nil (temp-table graph) :type 'void))))
      (assert (eq (temp-type ret-temp) ret-val) (ret-temp)
              "Return type ~A, isn't equal to the type specified in the ~
               function signature: ~A"
              (temp-type ret-temp) ret-val)
      (emit-op 'ret (list ret-temp) graph
               :operand-types (list (temp-type ret-temp)))
      (setf (ret graph) ret-temp))
    (setf (exit graph) (current graph))
    (classify-graph graph)
    graph))

(defun top-to-ir (expr env)
  "top level exprs will return flow-graphs but won't pass them"
  (case (car expr)
    ('defun (defun-to-ir (cdr expr) env))))

(defun exprs-to-ir (exprs)
  "abstract from froms input (file, string, etc..)"
  (let ((env (make-env)))
    (cons
     (loop for f in exprs
	       collect (if (eq (car f) 'defun)
		               (top-to-ir f env)
		               (error "only defun toplevel exprs supported at the moment")))
     env)))

(defun read-exprs-from-stream (stream)
  "`read` stream, so source code gets transexpred into lists of tokens"
  (loop for statement = (read stream nil)
        while statement
        collect statement))

(defun file-to-ir (file)
  (let* ((exprs (with-open-file (stream file)
                  (read-exprs-from-stream stream))))
    (exprs-to-ir exprs)))

(defun lasp-to-ir (name)
  "Convenience function to test files from the Lasp dir."
  (file-to-ir (opty-path (format nil "snippets/lasp/~A.lasp" name))))

;; (lasp-to-ir "maxcol")
;; (lasp-to-ir "simple-fns")
;; (serialize-ir-to-string (car (lasp-to-ir "simple-fns")))
#- (and) (with-input-from-string (s (serialize-ir-to-string (car (lasp-to-ir "simple-fns"))))
           (read-exprs-from-stream s))
