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

;;;; MORE PRIVATE CODE

;;;; Overriden methods of storable

(defmethod get-synchronized-data ((n note))
  (list (title n) (content n) (color n)))

(defmethod get-non-synchronized-data ((n note))
  (list (n-position n) (size n)))

(defmethod set-synchronized-data ((n note) data)
  (let ((title (first data))
	(content (second data))
	(color (third data)))
    (setf (title n) title)
    (setf (content n) content)
    (setf (color n) color)))

(defmethod set-non-synchronized-data ((n note) data)
  (let ((n-position (first data))
	(size (second data)))
    (setf (n-position n) n-position)
    (setf (size n) size)))
