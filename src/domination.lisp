(in-package :opty)

(defclass dominator (analysis)
  ((idoms         :initarg :idoms         :accessor idoms
                 :initform (make-hash-table))
   (dom-frontier :initarg :dom-frontier :accessor dom-frontier
                 :initform (make-hash-table))))

;; algorithm is from "A Simple, Fast Dominance Algorithm" paper by
;; Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy:
;; https://www.cs.tufts.edu/comp/150FP/archive/keith-cooper/dom14.pdf
(defun calculate-dom (graph)
  (let ((idoms (make-hash-table))
        (nodes (rpo-nodes graph)))
    (setf (gethash (id (car nodes)) idoms) (car nodes))
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
                                            (gethash (id p) idoms))
                                          predecessors)))
                  (setf predecessors (remove new-idom predecessors))
                  (loop
                    for p in predecessors
                    ;; if idom already exists
                    do (if (gethash (id p) idoms)
                           (setf new-idom (intersect p new-idom idoms))))
                  (unless (equal (gethash (id node) idoms) new-idom)
                    (setf (gethash (id node) idoms) new-idom)
                    (setf changed-p t))))))
    idoms))

(defun intersect (a b idoms)
  (let ((finger-a a)
        (finger-b b))
    (loop while (not (eq finger-a finger-b))
          do (progn
               (loop while (< (rpost finger-b) (rpost finger-a))
                     do (progn
                          (setf finger-a (gethash (id finger-a) idoms))))
               (loop while (< (rpost finger-a) (rpost finger-b))
                     do (progn
                          (setf finger-b (gethash (id finger-b) idoms))))))
    finger-a))

;; todo: actual test
(defun test-dom ()
  (let ((graph (make-classified-graph "maxcol" maxcol-graph)))
    (calculate-dom graph)))
