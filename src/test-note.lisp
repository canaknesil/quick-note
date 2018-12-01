(load (merge-pathnames "main.lisp" *load-truename*))
(use-package :canaknesil.quick-note-db-controller)
(use-package :canaknesil.quick-note-model)

(in-package :canaknesil.quick-note-model)

(defvar *note*
  (make-instance 'note
		 :title "Shopping List"
		 :content "Banana and orange."
		 :n-position (cons 5 5)
		 :size (cons 10 10)
		 :color 'blue))


		 
