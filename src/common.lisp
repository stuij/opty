(in-package :opty)

(defun opty-path (path)
  "returns path relative to the opty project root"
  (asdf:system-relative-pathname :opty path))

(defun counter ()
  (let ((i 0))
    (lambda ()
      (let ((out i))
        (incf i)
        out))))
