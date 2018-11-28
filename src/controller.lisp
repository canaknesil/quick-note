
;;;; Controller of MVC design.

;;;; TEMPLETE

(defun on-text-entry (text)
  "Called by the view when a line is entered. According to the text
entry use model and display text via view."
  (if (string= text "show")
      (show (get-text))
      (set-text text)))
