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

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))
