(in-package :canaknesil.quick-note-db-controller)

;;;; Part of the 'model' in MVC design.

;;;; DESIGN SPECIFICATION

;;;; Database controller for quick-note. Uses database.lisp.

;;;; Implements the database operations required by quick-note
;;;; model. Controles two databases, one ment to synchronized between
;;;; computers, other ment to be local. Abstracts the fact that there
;;;; is two seperate databases and provides a simple interface for
;;;; specifying which data is ment to be synchronized or not.

;;;; This module shouldn't be specific to storing notes, it should be
;;;; used to store any kind of information to the databases.

;;;; PRIVATE CODE

;;;; INTERFACE

(defclass storable ()
  ())

(defgeneric store2db (storable)
  (:documentation "Store instance to the quick-note database."))

(defmethod store2db ((s storable))
  ;; TODO
  (format t "Storing following object: (NOT IMPLEMENTED YET)~%~a~%"
	  s))

(defmethod load2db ((s storable))
  ;; TODO
  (format t "Loading following object: (NOT IMPLEMENTED YET)~%~a~%"
	  s))
