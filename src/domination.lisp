(in-package :opty)

(defclass domination (analysis)
  ((idoms        :initarg :idoms        :accessor idoms
                 :initform (make-hash-table))
   (children     :initarg :children     :accessor children
                 :initform (make-hash-table))
   (dom-frontier :initarg :dom-frontier :accessor dom-frontier
                 :initform (make-hash-table))))

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

(defun dominate-graph (graph)
  (let* ((idoms (calculate-idoms graph))
         (children (collect-children idoms))
         (dom-analysis (make-instance 'domination
                                      :idoms idoms
                                      :children children)))
    (setf (gethash 'domination (analyses graph))
          dom-analysis)))

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

(defun maxcol-graph-children ()
  (let ((idoms (calculate-idoms (make-classified-graph "maxcol" maxcol-graph))))
    (collect-children idoms)))

(define-test children
  (let* ((children (print-children (maxcol-graph-children))))
    (true (subsetp '(0 (0 1 5)) children :test 'equal))
    (true (subsetp '(1 (2 4)) children :test 'equal))
    (true (subsetp '(2 (6 3)) children :test 'equal))))
