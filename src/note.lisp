(in-package :canaknesil.quick-note-model)

;;;; Note data structure.

;;;; PRIVATE CODE

(defparameter *default-note-width* 20)
(defparameter *default-note-height* 20)

;;;; INTERFACE

(defclass note (storable)
  ((title :accessor title
	  :initarg :title
	  :initform "") ; string
   (content :accessor content
	    :initarg :content
	    :initform "") ; string
   (n-position :accessor n-position
	       :initarg :n-position
	       :initform (cons 0 0)) ; cons cell (x . y)
   (size :accessor size
	 :initarg :size
	 :initform (cons *default-note-width*
			 *default-note-height*)) ; cons cell (width . height)
   (color :accessor color
	  :initarg :color
	  :initform 'blue))) ; symbol

