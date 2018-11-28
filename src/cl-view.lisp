
;;;; View of MVC

;;;; TEMPLETE

(defun start ()
  "Start user interface. Read line entries and pass to the
controller. Exit when 'exit' is entered."
  (loop
     (format t "Enter text: ")
     (let ((text (read-line)))
       (if (string= text "exit") (return)
	   (on-text-entry text)))))

(defun show (text)
  "Display text on screen."
  (format t "~a~%" text))

