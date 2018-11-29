
;;;; This is where everything starts...

(defun load-with-truename (path)
  (load (merge-pathnames path *load-truename*)))

(defun load-list (list)
  (mapcar #'(lambda (p) (load-with-truename p)) list))

(load-list
 '("packages.lisp"
   "pathnames.lisp"
   "database.lisp"
   "note.lisp"
   "db-controller.lisp"
   "model.lisp"
   "ipc-back-end.lisp"
   "controller.lisp"))

(defun main ()
  (format t "Hello world!~%"))
