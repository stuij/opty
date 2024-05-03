(in-package :opty)

(defun opt-defun-to-ir (expr)
  expr)

(defun opt-exprs-to-ir (exprs)
  "abstract from froms input (file, string, etc..)"
  (loop for e in exprs
	    collect (if (eq (car e) 'defun)
		            (opt-defun-to-ir (cdr e))
		            (error "only defun toplevel exprs supported at the moment"))))

(defun opt-to-ir (name)
  "Convenience function to test files from the Opt dir."
  (file-to-ir (opty-path (format nil "snippets/opt/~A.opt" name))
              #'opt-exprs-to-ir))

;; (opt-to-ir "simple-fns")
;; (serialize-ir-to-string (car (opt-to-ir "simple-fns")))


