(in-package :opty)

;; IR op
(defclass op ()
  ((opcode    :initarg :opcode    :accessor opcode)
   (op-types :initarg  :op-types  :accessor op-types)
   (res-types :initarg :res-types :accessor res-types)
   (arity     :initarg :arity     :accessor arity)
   (source    :initarg :source    :accessor source)
   (operands  :initarg :operands  :accessor operands)
   (results   :initarg :results   :accessor results)
   (expr-temp-p :initarg :expr-temp-p :accessor expr-temp-p)))

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
     ,@(loop
         for op in op-list
         collect
         (destructuring-bind (opcode res-types op-types expr-temp-p) op
           (let ((fn-name (make-op-name opcode))
                 (arity (length op-types)))
             `(defun ,fn-name (operands &key results source
                                          result-types
                                          operand-types
                                          arity)
                (let ((arity (if arity
                                 arity
                                 ,arity))
                      (op-types (if operand-types
                                    operand-types
                                    ',op-types))
                      (res-types (if result-types
                                     result-types
                                     ',res-types)))
                  (assert (= (length operands) arity) (operands)
                          "Expecting ~A operands, ~A were given."
                          arity (length operands))
                  (assert (= (length operands) (length op-types))
                          (operands op-types)
                          "Expecting number of operands ~A ~
                                     to equal number of operand types: ~A"
                          (length operands) (length op-types))
                  (loop for op in operands
                        for type in op-types
                        do (assert (equalp (temp-type op) type) ()
                                   "Operand type ~A and expected ~
                                    type ~A are not equal"
                                   (temp-type op) type))
                  (if results
                      (loop for res in results
                            for type in res-types
                            do (assert (equalp (temp-type res) type) ()
                                   "Result type ~A and expected ~
                                    type ~A are not equal"
                                   (temp-type res) type)))
                  (make-instance 'op
                                 :opcode ',opcode
                                 :op-types op-types
                                 :res-types res-types
                                 :arity arity
                                 :source source
                                 :operands operands
                                 :expr-temp-p ,expr-temp-p
                                 :results results))))))))

;; list of (op-name, result types, operand types, expr-temp-p)
(gen-op-make-fns
 ((add    (i32)    (i32 i32)   t)
  (mul    (i32)    (i32 i32)   t)
  (lt     (i32)    (i32 i32)   t)
  (le     (i32)    (i32 i32)   t)
  (gt     (i32)    (i32 i32)   t)
  (ge     (i32)    (i32 i32)   t)
  (cpy    (union)  (union)     nil)
  (jmp    ()       (bb)        nil)
  (bcond  ()       (i32 bb bb) nil)
  (ret    ()       (union)     nil)
  ;; Amount of elem argumentscan vary as per array dimensions, which can't be
  ;; expressed in this implicit spec.
  ;; Data layout interpretation for elem arguments is row-major.
  (elem   (ptr)    (ptr i32)   t)
  (ldi    (i32)    (imm)       t)
  (ldp    (poison) ()          t)
  (ldr    (i32)    (ptr)       t)
  (str    ()       (i32 ptr)   t)))

(defun install-op (op graph &key source type-info (expr-temp-p t))
  "Register op in temp table, and append to current block"
  (with-slots (opcode operands res-types results) op
    (let* ((op-ident (op-to-ident opcode operands))
           (needs-result (and res-types (not results)))
           (ret (if needs-result
                    (to-temp op-ident (temp-table graph)
                             :source source
                             :expr-temp-p expr-temp-p
                             :type (op-result-type op)
                             :type-info type-info))))
      (if needs-result
          (setf results (list ret)))
      (append-op op graph)
      (values ret op))))

(defun emit-op (op args graph
                &key source result-types operand-types
                  type-info arity results (expr-temp-p t))
  (install-op (funcall (make-op-name op) args
                       :source source
                       :result-types result-types
                       :operand-types operand-types
                       :arity arity
                       :results results)
              graph
              :source source
              :type-info type-info
              :expr-temp-p expr-temp-p))

;; types
(defclass type-info ()
  ((temp-type :initarg :temp-type :accessor temp-type)))

(defclass pointer-info (type-info)
  ((pointee-type :initarg :pointee-type :accessor pointee-type)))

(defmethod print-ptr-type ((poi pointer-info))
  (format nil "~A" (pointee-type poi)))

(defclass array-info (pointer-info)
  ((pointee-type :initform 'arr)
   (dimensions   :initarg :dimensions :accessor dimensions)
   (arr-type     :initarg :arr-type   :accessor arr-type)))

(defmethod print-ptr-type ((arr array-info))
  (with-slots (pointee-type dimensions arr-type) arr
    (let ((index-names (loop for i in dimensions
                             collect (if (integerp i)
                                         i
                                         (print-ir i :typep nil)))))
      (format nil "(~A ~A (~{~A~^ ~}))"
              pointee-type arr-type index-names))))

(defmethod name ((n number))
  n)

(defmethod temp-type ((n number))
  'imm)

(defmethod print-ir ((n number) &key typep)
  (declare (ignore typep))
  (format nil "~A" n))

;; temps
;; we store IR temporaries in the graph/global temp table
(defclass temp ()
  ((name         :initarg :name         :reader   name)
   (op-ident     :initarg :op-ident     :reader   op-ident     :initform nil)
   (source       :initarg :source       :reader   source       :initform nil)
   (depends-info :initarg :depends-info :accessor depends-info :initform '())
   (temp-type    :initarg :temp-type    :accessor temp-type)
   (realm        :initarg :realm        :accessor realm)
   (type-info    :initarg :type-info    :accessor type-info    :initform nil)))

(defclass depends-info ()
  ((dependents       :initarg :dependents       :accessor dependents)
   (store-dependents :initarg :store-dependents :accessor store-dependents)
   ;; expression or value temporary
   (expr-temp-p     :initarg :expr-temp-p     :accessor expr-temp-p)))

(defclass temp-table ()
  ((table :initarg :table :accessor table :initform (make-hash-table :test 'equalp))
   (temps :initarg :temps :accessor temps :initform (make-hash-table))
   (temp-count :initarg :temp-count :accessor temp-count :initform (counter))))

(defun gen-temp-name (temp-table)
  (format-symbol t "%~S" (funcall (temp-count temp-table))))

(defun to-temp (op-ident temp-table
                &key source type name first type-info (expr-temp-p t))
  (with-slots (table temps) temp-table
    (if-let (temp (gethash op-ident table))
      (if first
          (error "No prior entry should exist in the temp table for: ~A" op-ident)
          temp)
      (let* ((temp-name (if name name (gen-temp-name temp-table)))
             (dep-info (make-instance 'depends-info :expr-temp-p expr-temp-p))
             (temp (setf (gethash op-ident table)
                         (make-instance 'temp :name temp-name
                                              :op-ident op-ident
                                              :source source
                                              :depends-info dep-info
                                              :temp-type type
                                              :type-info type-info))))
        (setf (gethash temp-name temps)
              temp)))))

(defmethod print-type ((tmp temp))
  (if (eq (temp-type tmp) 'ptr)
      (format nil "(~A ~A)" 'ptr (print-ptr-type (type-info tmp)))
      (format nil "~A" (temp-type tmp))))

(defmethod print-ir ((tmp temp) &key typep)
  (if (and typep *print-type*)
      (format nil "(~A ~A)" (name tmp) (print-type tmp))
      (format nil "~A" (name tmp))))

(defun new-temp (temp-table &key type type-info (expr-temp-p nil))
  "Usually used for compiler-generated temps."
  (let* ((name (gen-temp-name temp-table))
         (dep-info (make-instance 'depends-info :expr-temp-p expr-temp-p))
         (temp (make-instance 'temp :name name
                                    :temp-type type
                                    :type-info type-info
                                    :depends-info dep-info)))
    (setf (gethash name (temps temp-table))
          temp)))

(defun op-to-ident (op args)
  (append (list op)
          (loop for a in args
                collect (name a))))


;; basic block
(defclass basic-block (classify-node)
  ((instrs :initarg :instrs :accessor instrs :initform '())))

(defmethod name ((bb basic-block))
  (bb-symbol bb))

(defun bb-symbol (basic-block)
  (make-keyword (bb-name basic-block)))

(defun bb-name (basic-block)
  (format nil "BB-~A" (id basic-block)))

(defmethod temp-type ((bb basic-block))
  'bb)

(defun create-bb (graph)
  (let ((bb (make-instance 'basic-block :id (funcall (bb-count graph)))))
    (setf (gethash (id bb) (nodes graph))
          bb)))

(defmethod print-ir ((bb basic-block) &key typep)
  (declare (ignore typep))
  (format nil ":~A" (bb-symbol bb)))

;; graph
(defclass flow-graph (digraph)
  ((args       :initarg :args       :accessor args     :initform '())
   (ret        :initarg :ret        :accessor ret      :initform nil)
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

(defun make-initial-analyses (graph)
  (classify-graph graph)
  (dominate-graph graph))


;; serialize
(defparameter *print-source* t)
(defparameter *print-type* t)

(defun serialize-arg (s arg colon at)
  (declare (ignore colon at))
  (format s "~A" (print-ir arg :typep t)))

(defun serialize-arguments (graph s)
  (format s "(")
  (format s "~{~/opty:serialize-arg/~^ ~}" (args graph))
  (format s ")"))

(defun serialize-op (op s)
  (let* ((opcode (opcode op))
         (operands (loop for o in (operands op)
                         collect (print-ir o :typep t)))
         (results (loop for r in (results op)
                        collect (if r (print-ir r :typep t))))
         (source (source op))
         ;; TODO: make some kind of regex to strip and replace whitespace
         (one-line-source (replace-all (substitute #\space #\newline
                                                   (format nil "~A" source))
                                       "    " "")))
    (if results
        (format s "~%    (~A ~{~A~^ ~} ~{~A~^ ~})"
                opcode results operands)
        (format s "~%    (~A ~{~A~^ ~})"
                opcode operands))
    (if (and source *print-source*)
        (format s " ;; ~A" one-line-source)
        (format s ""))))

(defun serialize-blocks (graph s)
  (loop for node in (rpo-nodes graph)
        do (progn
             (format s "~%  ~A" (print-ir node))
             (loop for op in (instrs node)
                   do (serialize-op op s)))))

(defun serialize-func (s graph colon at)
  (declare (ignore colon at))
  (format s "(defun ~A " (label graph))
  (format s "~A " (print-type (ret graph)))
  (serialize-arguments graph s)
  (serialize-blocks graph s)
  (format s ")"))

(defun serialize-ir (ir s)
  (let ((*print-case* :downcase))
    (format s "~{~/opty:serialize-func/~^~%~%~}" ir)))

(defun serialize-ir-to-string (ir)
  "as opposed to say a file"
  (with-output-to-string (s)
    (serialize-ir ir s)))
