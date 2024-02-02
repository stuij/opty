(in-package :opty)

;; IR op
(defclass op ()
  ((opcode   :initarg :opcode   :reader   opcode)
   ;; first type is result, rest are operands in order
   (types    :initarg :types    :reader   types)
   (arity    :initarg :arity    :reader   arity)
   (operands :initarg :operands :accessor operands)
   (result   :initarg :result   :accessor result)))

(defun op-result-type (op)
  (car (types op)))

(defun op-operand-types (op)
  (cdr (types op)))

(defun append-op (op graph)
  (with-slots (instrs) (current graph)
    (setf instrs (append instrs (list op)))))

(defun make-op-name (op)
  (format-symbol t "MAKE-OP-~A"
                 (symbol-name op)))

(defmacro gen-op-make-fns (op-list)
  `(progn
     ,@(loop for op in op-list
             collect (destructuring-bind (opcode arity types) op
                       (let ((fn-name (make-op-name opcode)))
                         `(defun ,fn-name (operands &optional result)
                            (assert (= (length operands) ,arity) (operands)
                                    "Expecting ~A operands, ~A were given."
                                    ,arity (length operands))
                            (make-instance
                             'op
                             :opcode ',opcode
                             :types ,types
                             :arity ,arity
                             :operands operands
                             :result result)))))))

(gen-op-make-fns
 ((iadd 2 '(i32 i32 i32))
  (imul 2 '(i32 i32 i32))))

;; Mapping of source language to opcode name for basic ops.
(defparameter *src-to-plain-ops-table*
  '((add . iadd) (mul . imul)))


;; temps
;; we store IR temporaries in the graph/global temp table
(defclass temp ()
  ((name      :initarg :name      :reader   name)
   (op-ident  :initarg :op-ident  :reader   op-ident)
   (source    :initarg :source    :reader   source :initform nil)
   (temp-type :initarg :temp-type :accessor temp-type)))

(defclass temp-table ()
  ((table :initarg :table :accessor table :initform (make-hash-table :test 'equalp))
   (temps :initarg :temps :accessor temps :initform (make-hash-table))
   (temp-count :initarg :temp-count :accessor temp-count :initform (counter))))

(defun gen-temp-name (temp-table)
  (format-symbol t "TMP-~S" (funcall (temp-count temp-table))))

(defun to-temp (op-ident temp-table &key source type name first)
  (with-slots (table temps) temp-table
    (if-let (temp (gethash op-ident table))
      (if first
          (error "No prior entry should exist in the temp table for: ~A" op-ident)
          temp)
      (let* ((temp-name (if name name (gen-temp-name temp-table)))
             (temp (setf (gethash op-ident table)
                         (make-instance 'temp :name temp-name
                                              :op-ident op-ident
                                              :source source
                                              :temp-type type))))
        (setf (gethash temp-name temps)
              temp)))))


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


;; basic block
(defclass basic-block (digraph-node)
  ((instrs :initarg :instrs :accessor instrs :initform '())))

(defun create-block (id)
  (make-instance 'basic-block :id id))


;; graph
(defclass flow-graph (digraph)
  ((args       :initarg :args       :accessor args     :initform '())
   (bb-count   :initarg :bb-counter :accessor bb-count :initform (counter))
   (current    :initarg :current    :accessor current)
   (temp-table :initarg :temp-table :accessor temp-table
               :initform (make-instance 'temp-table))))

(defmethod start-block ((graph flow-graph) (block basic-block))
  (setf (current graph) block))

(defun initialize-graph (label)
  (let* ((entry (create-block 0))
         (exit (create-block 1))
	     (graph (make-instance
		         'flow-graph :entry entry :exit exit
		         :nodes (list entry exit)
		         :label label)))
    (setf (successors entry) (list exit))
    (setf (predecessors exit) (list entry))
    (start-block graph entry)
    graph))

(defun op-to-ident (op args)
  (append (list op)
          (loop for a in args
                collect (name a))))

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
                     for expr in body
                     do (setf ret (to-flow expr env graph))
                     return ret)))
      (cons graph ret))))

;; defun-to-flow todo:
;; - bind ret of last expr to fn return value

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
  (file-to-flow-graph (opty-path (with-output-to-string (s)
                                   (format s "snippets/lasp/~A.lasp" name)))))

;; (to-ir "maxcol")
;; (to-ir "add")
