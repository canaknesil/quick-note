(in-package :canaknesil.quick-note-view)
(in-readtable :qtools) ; also include this to each view file

;;;; Note manager view.

;;;; TEMPLETE

(define-widget main-window (QWidget)
  ())

(define-subwidget (main-window name) (q+:make-qlineedit main-window)
  (setf (q+:placeholder-text name) "Your name please."))

(define-subwidget (main-window go) (q+:make-qpushbutton "Go!" main-window))

(define-subwidget (main-window layout) (q+:make-qhboxlayout main-window)
  (q+:add-widget layout name)
  (q+:add-widget layout go))

(define-signal (main-window name-set) (string))

(define-slot (main-window go) ()
  (declare (connected go (pressed)))
  (declare (connected name (return-pressed)))
  (signal! main-window (name-set string) (q+:text name)))

(define-slot (main-window name-set) ((new-name string))
  (declare (connected main-window (name-set string)))
  (q+:qmessagebox-information main-window
			      "Greetings"
			      (format NIL "Good day to you, ~a!" new-name)))

(defun launch-note-manager ()
  (with-main-window (window 'main-window)))
