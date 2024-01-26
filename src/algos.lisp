(in-package :opty)

(defun graph-edges (graph)
  (loop for e in graph
        append (car (cdr e))))

(defun graph-nodes (graph)
  (loop for e in graph
        collect (car e)))

(defclass digraph-node ()
  ((id         :initarg :id         :accessor id)
   (successors :initarg :successors :accessor successors)))

(defmethod print-object ((obj digraph-node) out)
  (with-slots (id) obj
    (print-unreadable-object (obj out :type t)
      (format out "id:~A" id))))

(defclass digraph ()
  ((label  :initarg :label  :accessor label)
   (nodes :initarg :nodes :accessor nodes)
   (entry :initarg :entry :accessor entry)))


(defun initialize-digraph-node (id)
  (make-instance 'digraph-node :id id
                               :successors '()))

(defun to-digraph (label graph &optional (initializer #'initialize-digraph-node))
  (let* ((nodes (make-hash-table))
         (node-ids (graph-nodes graph))
         (edges (graph-edges graph)))
    (loop for id in node-ids
          do (setf (gethash id nodes)
                   (funcall initializer id)))
    (loop for e in edges
          do (let ((origin (car e))
                   (successor (cdr e)))
               (setf (successors (gethash origin nodes))
                     (append (successors (gethash origin nodes))
                             (list (gethash successor nodes))))))
    (make-instance 'digraph :label label
                            :nodes nodes
                            :entry (gethash (car (car graph)) nodes))))

(defclass classify-node (digraph-node)
  ((pre   :initarg :pre   :accessor pre   :initform 0)
   (rpost :initarg :rpost :accessor rpost :initform 0)))

(defun initialize-classify-node (id)
  (make-instance 'classify-node :id id
                                :successors '()))

(defclass edge-classification ()
  ((tree-edges    :initarg :tree-edges    :accessor tree-edges    :initform '())
   (forward-edges :initarg :forward-edges :accessor forward-edges :initform '())
   (cross-edges   :initarg :cross-edges   :accessor cross-edges   :initform '())
   (back-edges    :initarg :back-edges    :accessor back-edges    :initform '())))

(defun classify-graph (digraph)
  (let* ((nodes (nodes digraph))
         (preorder 1)
         (rpostorder (hash-table-count nodes))
         (classification (make-instance 'edge-classification)))
    (labels ((dfs (node)
               (setf (pre node) preorder
                     preorder (1+ preorder))
               (loop for s in (successors node)
                     do (cond ((= (pre s) 0)
                               (setf (tree-edges classification)
                                     (append (tree-edges classification)
                                             `((,(id node) . ,(id s)))))
                               (dfs s))
                              ((= (rpost s) 0)
                               (setf (back-edges classification)
                                     (append (back-edges classification)
                                             `((,(id node) . ,(id s))))))
                              ((< (pre node) (pre s))
                               (setf (forward-edges classification)
                                     (append (forward-edges classification)
                                             `((,(id node) . ,(id s))))))
                              (t
                               (setf (cross-edges classification)
                                     (append (cross-edges classification)
                                             `((,(id node) . ,(id s))))))))
               (setf (rpost node) rpostorder
                     rpostorder (1- rpostorder))))
      (dfs (entry digraph))
      classification)))

;; testing
(defparameter maxcol-graph
  '((0 ((0 . 1) (0 . 5))) ;; B0
    (1 ((1 . 2) (1 . 4))) ;; B1
    (2 ((2 . 3) (2 . 6))) ;; B2
    (3 ((3 . 2) (3 . 4))) ;; B3
    (4 ((4 . 1) (4 . 5))) ;; B4
    (5 ())                ;; B5
    (6 ((6 . 3)))))       ;; B6

(define-test classify-graph
  (let* ((digraph (to-digraph "maxcol" maxcol-graph #'initialize-classify-node))
         (classification (classify-graph digraph)))
    (true (subsetp (tree-edges classification)
                   '((0 . 1) (1 . 2) (2 . 3) (3 . 4) (4 . 5) (2 . 6))
                   :test 'equal))
    (true (subsetp (forward-edges classification)
                   '((1 . 4) (0 . 5))
                   :test 'equal))
    (true (subsetp (cross-edges classification)
                   '((6 . 3))
                   :test 'equal))
    (true (subsetp (back-edges classification)
                   '((3 . 2) (4 . 1))
                   :test 'equal))))
