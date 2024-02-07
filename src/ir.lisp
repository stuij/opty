(in-package :opty)

;; IR op
(defclass op ()
  ((opcode    :initarg :opcode    :reader   opcode)
   (op-types :initarg  :op-types  :reader   op-types)
   (res-types :initarg :res-types :reader   res-types)
   (arity     :initarg :arity     :reader   arity)
   (source    :initarg :source    :accessor source)
   (operands  :initarg :operands  :accessor operands)
   (results   :initarg :results   :accessor results)))

(defun op-result-type (op)
  (assert (res-types op) ()
          "This op doesn't have a result register: ~A" op)
  (car (res-types op)))

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
             collect (destructuring-bind (opcode op-types res-types) op
                       (let ((fn-name (make-op-name opcode))
                             (arity (length op-types)))
                         `(defun ,fn-name (operands &key results source)
                            (assert (= (length operands) ,arity) (operands)
                                    "Expecting ~A operands, ~A were given."
                                    ,arity (length operands))
                            (make-instance
                             'op
                             :opcode ',opcode
                             :op-types ',op-types
                             :res-types ',res-types
                             :arity ,arity
                             :source source
                             :operands operands
                             :results results)))))))

;; list of op-name, operand types, result types
(gen-op-make-fns
 ((iadd (i32 i32) (i32))
  (imul (i32 i32) (i32))
  (ilt  (i32 i32) (i32))
  (icpy (i32 i32) ())
  (jmp  (label) ())
  (ibcond (i32 label label) ())))

(defun install-op (op graph &optional source)
  "Register op in temp table, and append to current block"
  (with-slots (opcode operands res-types) op
    (let* ((op-ident (op-to-ident opcode operands))
           (ret (if res-types
                    (to-temp op-ident (temp-table graph)
                             :source source
                             :type (op-result-type op)))))
      (if res-types
          (setf (results op) (list ret)))
      (append-op op graph)
      ret)))

(defun emit-op (op args expr graph)
  (install-op (funcall (make-op-name op) args :source expr)
              graph
              expr))


;; temps
;; we store IR temporaries in the graph/global temp table
(defclass temp ()
  ((name      :initarg :name      :reader   name)
   (op-ident  :initarg :op-ident  :reader   op-ident :initform nil)
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

(defun new-temp (temp-table &optional type)
  "Usually used for compiler-generated temps."
  (let* ((name (gen-temp-name temp-table))
         (temp (make-instance 'temp :name name
                                   :temp-type type)))
    (setf (gethash name (temps temp-table))
          temp)))

(defun op-to-ident (op args)
  (append (list op)
          (loop for a in args
                collect (name a))))


;; basic block
(defclass basic-block (classify-node)
  ((instrs :initarg :instrs :accessor instrs :initform '())))

(defmethod name ((b basic-block))
  (bb-symbol b))

(defun create-bb (graph)
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

(defmethod start-block ((block basic-block) (graph flow-graph))
  (setf (current graph) block))

(defun initialize-graph (label)
  (let* ((graph (make-instance
		         'flow-graph :label label))
         (entry (create-bb graph)))
    (setf (entry graph) entry)
    (start-block entry graph)
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
  (let* ((opcode (opcode op))
         (operands (loop for o in (operands op)
                         collect (name o)))
         (results (loop for r in (results op)
                        collect (if r (name r))))
         (source (source op))
         ;; TODO: make some kind of regex to strip and replace whitespace
         (one-line-source (replace-all (substitute #\space #\newline
                                                   (format nil "~A" source))
                                       "    " "")))
    (if results
        (format s "    (~A ~{~A~^ ~} ~{~A~^ ~}) ;; ~A~%"
                opcode results operands one-line-source)
        (format s "    (~A ~{~A~^ ~}) ;; ~A~%"
                opcode operands one-line-source))))

(defun serialize-blocks (graph s)
  (loop for node being the hash-value of (nodes graph)
        do (progn
             (format s "  :~A~%"(bb-symbol node))
             (loop for op in (instrs node)
                   do (serialize-op op s)))))

(defun serialize-func (s graph colon at)
  (declare (ignore colon at))
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
