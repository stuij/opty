(in-package :opty)

;; a literal graph is meant to be easily constructed from text,
;; which can then be parsed into a more structured binary representation
;;
;; breakdown:
;; - a list of
;;   - a list of
;;     - the node number
;;     - a list of edges to other nodes represented as tuples
;;       (successors in a directed graph)
;;   - a tuple of the entry and exit node
;;
;; example:
;;
;; (defparameter maxcol-graph
;;   '(((0 ((0 . 1) (0 . 5))) ;; B0
;;      (1 ((1 . 2) (1 . 4))) ;; B1
;;      (2 ((2 . 3) (2 . 6))) ;; B2
;;      (3 ((3 . 2) (3 . 4))) ;; B3
;;      (4 ((4 . 1) (4 . 5))) ;; B4
;;      (5 ())                ;; B5
;;      (6 ((6 . 3))))        ;; B6
;;     (0 . 5)))              ;; entry and exit node respectively

(defun graph-edges (graph)
  (loop for e in (car graph)
        append (car (cdr e))))

(defun graph-nodes (graph)
  (loop for e in (car graph)
        collect (car e)))

(defun graph-entry (graph)
  (caadr graph))

(defun graph-exit (graph)
  (cdadr graph))

;; digraph
(defclass digraph-node ()
  ((id         :initarg :id         :accessor id)
   (successors :initarg :successors :accessor successors)))

(defmethod print-object ((obj digraph-node) out)
  (with-slots (id) obj
    (print-unreadable-object (obj out :type t)
      (format out "id:~A" id))))

(defclass digraph ()
  ((label    :initarg :label    :accessor label)
   (nodes    :initarg :nodes    :accessor nodes)
   (entry    :initarg :entry    :accessor entry)
   (exit     :initarg :exit     :accessor exit)
   (analyses :initarg :analyses :accessor analyses
             :initform (make-hash-table))))

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
                            :entry (gethash (graph-entry graph) nodes)
                            :exit (gethash (graph-exit graph) nodes))))

;; A classified digraph is a digraph that uses classify nodes, which add pre the
;; and rpost slots to the nodes, which relate to their pre and reverse post
;; order DFS number.
;;
;; These are populated in the classify-graph function. This function will also
;; return a classification analysis, which will classify the nodes as tree edges,
;; forward edges, cross edges or back edges.
;;
;; The `make-classified-graph` function will take a literal graph and name, will
;; make a classification digraph, will do the classification and will store the
;; classification analysis in the `analysis` hash table of the digraph.
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

(defun make-classified-graph (name graph)
  (let ((digraph (to-digraph name graph #'initialize-classify-node)))
    (setf (gethash 'classification  (analyses digraph))
          (classify-graph digraph))
    digraph))

;; testing
(defparameter maxcol-graph
  '(((0 ((0 . 1) (0 . 5))) ;; B0
     (1 ((1 . 2) (1 . 4))) ;; B1
     (2 ((2 . 3) (2 . 6))) ;; B2
     (3 ((3 . 2) (3 . 4))) ;; B3
     (4 ((4 . 1) (4 . 5))) ;; B4
     (5 ())                ;; B5
     (6 ((6 . 3))))        ;; B6
    (0 . 5)))              ;; entry and exit node respectively

(define-test classify-graph
  (let* ((digraph (make-classified-graph "maxcol" maxcol-graph))
         (classification (gethash 'classification (analyses digraph))))
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

;; graph node ordering
(defun rpo-nodes (digraph)
  "Return reverse post-order nodes of classified graph."
  (loop for v being the hash-values of (nodes digraph)
        collect v into values
        finally (return (sort values (lambda (a b)
                                       (< (rpost a) (rpost b)))))))

(define-test reverse-post-order
  (let* ((digraph (make-classified-graph "maxcol" maxcol-graph))
         (node-nrs (loop for n in (rpo-nodes digraph)
                         collect (id n))))
    (true (equal node-nrs
                 '(0 1 2 6 3 4 5)))))
