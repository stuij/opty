(in-package :opty)

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
