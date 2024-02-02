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


;; basic block
(defclass basic-block (digraph-node)
  ((instrs :initarg :instrs :accessor instrs :initform '())))

(defun create-block (id)
  (make-instance 'basic-block :id id))

;; graph
(defclass flow-graph (digraph)
  ((bb-count :initarg :bb-counter :accessor bb-count :initform (counter))
   (current  :initarg current :accessor current)))

(defmethod start-block ((graph flow-graph) (block basic-block))
  (setf (current graph) block))

(defun initialize-graph (label)
  (let* ((entry (create-block 0))
         (exit (create-block nil))
	 (graph (make-instance
		 'flow-graph :entry entry :exit exit
		 :nodes (list entry exit)
		 :label label)))
    (start-block graph entry)
    graph))

(defun defun-to-flow (exp env)
  (let ((graph (initialize-graph (string-downcase (string (car exp))))))
    graph))

(defun to-flow (form env)
  (case (car form)
    ('defun (defun-to-flow (cdr form) env))))

(defun forms-to-flowgraph (forms)
  "abstract from froms input (file, string, etc..)"
  (let ((env '()))
    (loop for f in forms
	  collect (if (eq (car f) 'defun)
		      (to-flow f env)
		      (error "only defun toplevel forms supported at the moment")))))

(defun read-forms-from-stream (stream)
  "`read` stream, so source code gets transformed into lists of tokens"
  (loop for statement = (read stream nil)
        while statement
        collect statement))

(defun file-to-flow-graph (file)
  (let* ((forms (with-open-file (stream file)
                  (read-forms-from-stream stream))))
    (forms-to-flowgraph forms)))

(defun read-maxcol ()
  (file-to-flow-graph (opty-path "snippets/maxcol.opt")))
