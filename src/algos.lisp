(in-package :opty)

(defparameter maxcol-graph
  '(((0 . 1) (0 . 5)) ;; B0
    ((1 . 4) (1 . 2)) ;; B1
    ((2 . 3) (2 . 6)) ;; B2
    ((3 . 2) (3 . 4)) ;; B3
    ((4 . 1) (4 . 5)) ;; B4
    ()                ;; B5
    ((6 . 3))))       ;; B6

(defun graph-edges (graph)
  (loop for e in graph
        append e))

(defun graph-nodes (graph)
  (remove-duplicates
   (loop for e in (graph-edges graph)
         append `(,(car e) ,(cdr e)))))
