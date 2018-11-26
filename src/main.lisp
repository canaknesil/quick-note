
;;;; This is where everything starts...

(defun load-with-truename (path)
  (load (merge-pathnames path *load-truename*)))

(load-with-truename "packages.lisp")

(load-with-truename "pathnames.lisp")
(load-with-truename "database.lisp")

(defun main ()
  (format t "Hello World!~%"))

