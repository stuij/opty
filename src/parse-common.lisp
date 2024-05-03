(in-package :opty)

(defun read-exprs-from-stream (stream)
  "`read` stream, so source code gets transexpred into lists of tokens"
  (loop for statement = (read stream nil)
        while statement
        collect statement))

(defun file-to-ir (file processor)
  (let* ((exprs (with-open-file (stream file)
                  (read-exprs-from-stream stream))))
    (funcall processor exprs)))
