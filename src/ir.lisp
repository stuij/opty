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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-op-name (op)
    (format-symbol t "MAKE-OP-~A"
                   (symbol-name op))))

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

(defun op-to-ident (op args)
  (append (list op)
          (loop for a in args
                collect (name a))))


;; basic block
(defclass basic-block (classify-node)
  ((instrs :initarg :instrs :accessor instrs :initform '())))

(defun create-block (graph)
  (let ((bb (make-instance 'basic-block :id (funcall (bb-count graph)))))
    (setf (gethash (id bb) (nodes graph))
          bb)))

(defun bb-symbol (basic-block)
  (make-keyword (bb-name basic-block)))

(defun bb-name (basic-block)
  (format nil "BB-~A" (id basic-block)))


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
  (let* ((graph (make-instance
		         'flow-graph :label label))
         (entry (create-block graph))
         (exit (create-block graph)))
    (setf (entry graph) entry)
    (setf (exit graph) exit)
    (setf (successors entry) (list exit))
    (setf (predecessors exit) (list entry))
    (start-block graph entry)
    graph))


;; serialize
(defun serialize-arg (s arg colon at)
  (declare (ignore colon at))
  (format s "~A" (name arg)))

(defun serialize-arguments (graph s)
  (format s "(")
  (format s "~{~/opty:serialize-arg/~^ ~}" (args graph))
  (format s ")~%"))

(defun serialize-op (op s)
  (let ((op (opcode op))
        (operands (loop for o in (operands op)
                        collect (name o)))
        (result (name (result op)))
        (source (source (result op))))
    (format s "    (~A ~A ~{~A~^ ~}) ;; ~A~%" op result operands source)))

(defun serialize-blocks (graph s)
  (loop for node in (rpo-nodes graph)
        do (progn
             (format s "  :~A~%"(bb-symbol node))
             (loop for op in (instrs node)
                   do (serialize-op op s)))))

(defun serialize-func (s graph colon at)
  (declare (ignore colon at))
  ;; make sure our tree edge classification is up-to-date
  (classify-graph graph)
  (format s "(defun ~A " (label graph))
  (serialize-arguments graph s)
  (serialize-blocks graph s)
  (format s ")"))

(defun serialize-ir (ir s)
  (format s "~{~/opty:serialize-func/~^~%~%~}" ir))

(defun serialize-ir-to-string (ir)
  "as opposed to say a file"
  (with-output-to-string (s)
    (serialize-ir ir s)))
