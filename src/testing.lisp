(in-package :opty)

(defun run-tests ()
  (test 'classify-graph)
  (test 'reverse-post-order)
  (test 'idoms)
  (test 'dom-frontiers))
