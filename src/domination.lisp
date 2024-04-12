(in-package :opty)

(defclass domination (analysis)
  ((idoms         :initarg :idoms         :accessor idoms)
   (children      :initarg :children      :accessor children)
   (dom-frontiers :initarg :dom-frontiers :accessor dom-frontiers)))

;; algorithm is from "A Simple, Fast Dominance Algorithm" paper by
;; Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy:
;; https://www.cs.tufts.edu/comp/150FP/archive/keith-cooper/dom14.pdf
(defun calculate-idoms (graph)
  (let ((idoms (make-hash-table))
        (nodes (rpo-nodes graph)))
    (setf (gethash (car nodes) idoms) (car nodes))
    (loop
      with changed-p = t
      while changed-p
      do (progn
           (setf changed-p nil)
           (loop
             with new-idom = nil
             ;; remove the root note, which should be the first in the
             ;; reverse post-order list
             for node in (cdr nodes)
             do (let* ((predecessors (predecessors node))
                       ;; find a predecessor that has an idom
                       (new-idom (find-if (lambda (p)
                                            (gethash p idoms))
                                          predecessors)))
                  (setf predecessors (remove new-idom predecessors))
                  (loop
                    for p in predecessors
                    ;; if idom already exists
                    do (if (gethash p idoms)
                           (setf new-idom (intersect p new-idom idoms))))
                  (unless (equal (gethash node idoms) new-idom)
                    (setf (gethash node idoms) new-idom)
                    (setf changed-p t))))))
    idoms))

(defun intersect (a b idoms)
  (let ((finger-a a)
        (finger-b b))
    (loop while (not (eq finger-a finger-b))
          do (progn
               (loop while (< (rpost finger-b) (rpost finger-a))
                     do (setf finger-a (gethash finger-a idoms)))
               (loop while (< (rpost finger-a) (rpost finger-b))
                     do (setf finger-b (gethash finger-b idoms)))))
    finger-a))

(defun print-idoms (idoms)
  (loop for v being the hash-values of idoms
        using (hash-key k)
        collect (cons (id k) (id v))))

(define-test idoms
  (let* ((idoms (calculate-idoms (make-classified-graph "maxcol" maxcol-graph)))
         (idoms-list (print-idoms idoms)))
    (true (subsetp '((0 . 0)) idoms-list :test 'equal))
    (true (subsetp '((1 . 0)) idoms-list :test 'equal))
    (true (subsetp '((2 . 1)) idoms-list :test 'equal))
    (true (subsetp '((6 . 2)) idoms-list :test 'equal))
    (true (subsetp '((3 . 2)) idoms-list :test 'equal))
    (true (subsetp '((4 . 1)) idoms-list :test 'equal))
    (true (subsetp '((5 . 0)) idoms-list :test 'equal))))

(defun collect-children (idoms)
  (loop for parent being the hash-values of idoms
          using (hash-key child)
        with children = (make-hash-table)
        do (setf (gethash parent children)
                 (append (gethash parent children) (list child)))
        finally (return children)))

(defun print-children (children)
  (loop for childs being the hash-values of children
        using (hash-key parent)
        collect (list (id parent)
                      (loop for c in childs
                            collect (id c)))))

(defun maxcol-graph-children ()
  (let ((idoms (calculate-idoms (make-classified-graph "maxcol" maxcol-graph))))
    (collect-children idoms)))

(define-test children
  (let* ((children (print-children (maxcol-graph-children))))
    (true (subsetp '((0 (0 1 5))) children :test 'equal))
    (true (subsetp '((1 (2 4))) children :test 'equal))
    (true (subsetp '((2 (6 3))) children :test 'equal))))

(defun calculate-dominance-frontiers (bbs idoms)
  (loop
    with dfs = (make-hash-table)
    for bb being the hash-values of bbs
    for preds = (predecessors bb)
    when (> (length preds) 1)
      do (loop for p in preds
               do (do ((runner p (gethash runner idoms)))
                      ((eql runner (gethash bb idoms)))
                    (unless (member bb (gethash runner dfs))
                      (push bb (gethash runner dfs)))))
    finally (return dfs)))

(defun print-dom-frontiers (frontiers)
  (loop for childs being the hash-values of frontiers
        using (hash-key parent)
        collect (list (id parent)
                      (loop for c in childs
                            collect (id c)))))



(defun maxcol-frontiers ()
  (let* ((graph (make-classified-graph "maxcol" maxcol-graph))
         (idoms (calculate-idoms graph)))
    (calculate-dominance-frontiers (nodes graph) idoms)))

(define-test dom-frontiers
  (let ((frontiers (print-dom-frontiers (maxcol-frontiers))))
    (format t "frontiers: ~A" frontiers)
    (true (subsetp '((4 (5 1))) frontiers :test 'equalp))
    (true (subsetp '((1 (5 1))) frontiers :test 'equal))
    (true (subsetp '((3 (4 2))) frontiers :test 'equal))
    (true (subsetp '((2 (4 2))) frontiers :test 'equal))
    (true (subsetp '((6 (3)))   frontiers :test 'equal))))

(defun calculate-iterated-dom-frontier (nodes dom-frontiers)
  (do* ((df+ (copy-list nodes))
        (worklist (copy-list nodes))
        (curr))
      ((emptyp worklist)
       df+)
    (setf curr (pop worklist))
    (loop for bb in (gethash curr dom-frontiers)
          do (if (not (member bb df+))
                 (progn
                   (push bb df+)
                   (push bb worklist))))))

(defun ids-to-nodes (ids graph)
  (loop for id in ids
        collect (gethash id (nodes graph))))

(defun maxcol-iterated-dom-frontier (ids)
  (let* ((graph (make-classified-graph "maxcol" maxcol-graph))
         (idoms (calculate-idoms graph))
         (frontiers (calculate-dominance-frontiers (nodes graph) idoms))
         (node-list (ids-to-nodes ids graph)))
    (calculate-iterated-dom-frontier node-list frontiers)))

(defun print-iterated-dom-frontier (idf)
  (loop for bb in idf
        collect (id bb)))

(defun print-node-ids-to-idf (node-ids frontiers graph)
  (print-iterated-dom-frontier
   (calculate-iterated-dom-frontier (ids-to-nodes node-ids graph) frontiers)))

(define-test iterated-dom-frontier
  (let* ((graph (make-classified-graph "maxcol" maxcol-graph))
         (idoms (calculate-idoms graph))
         (frontiers (calculate-dominance-frontiers (nodes graph) idoms)))
    (true (equal '(0) (print-node-ids-to-idf '(0) frontiers graph)))
    (true (equal '(1 5 2 4 3 6) (print-node-ids-to-idf '(6) frontiers graph)))
    (true (equal '(1 5 4 2) (print-node-ids-to-idf '(2) frontiers graph)))
))

(defun dominate-graph (graph)
  (let* ((idoms (calculate-idoms graph))
         (children (collect-children idoms))
         (dom-frontiers (calculate-dominance-frontiers (nodes graph) idoms))
         (dom-analysis (make-instance 'domination
                                      :idoms idoms
                                      :children children
                                      :dom-frontiers dom-frontiers)))
    (setf (gethash 'domination (analyses graph))
          dom-analysis)))
