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

(defvar *synchronized-db* nil) ; database object from database.lisp
(defvar *non-synchronized-db* nil)

(defparameter *synchronized-db-name* "quick-note-synchronized-db")
(defparameter *non-synchronized-db-name*
  "quick-note-non-synchronized-db")


;;;; INTERFACE

(defclass storable ()
  ((name :accessor name
	 :initarg :name
	 :initform nil) ; string
   (hierarchy :accessor hierarchy
	      :initarg :hierarchy
	      :initform nil))) ; list of strings

(defgeneric store2db (storable)
  (:documentation "Store instance to the quick-note database."))

(defgeneric load-from-db (storable)
  (:documentation "Load instance from quick-note-database."))

(defgeneric get-synchronized-data (storable)
  (:documentation
   "To be overriden by subclasses. Every subclass should return data
to be stored and synchronized between computers."))

(defgeneric get-non-synchronized-data (storable)
  (:documentation
   "To be overriden by subclasses. Every subclass should return data
to be stored but not synchronized between computers."))

(defgeneric set-synchronized-data (storable data)
  (:documentation
   "To be overriden by subclasses. Every subclass should install the
data which is synchronized between computers to the instance during
loading."))

(defgeneric set-non-synchronized-data (storable data)
  (:documentation
   "To be overriden by subclasses. Every subclass should install the
data which is not synchronized between computers to the instance
during loading."))

;;; For now these four function are repetitive.
(defun set-synchronized-db (path)
  "Sets the existing synchronized database in the given
directory. Returns t if successful, nil if not."
  (let ((db (get-database path *synchronized-db-name*)))
    (if db (progn (setf *synchronized-db* db) t)
	nil)))
	
(defun set-non-synchronized-db (path)
  "Sets the existing non-synchronized database in the given
directory. Returns t if successful, nil if not."
  (let ((db (get-database path *non-synchronized-db-name*)))
    (if db (progn (setf *non-synchronized-db* db) t)
	nil)))

(defun create-synchronized-db (path)
  "Creates the non-existing synchronized database in the given
directory. Returns t if successfull, nil if not."
  (if (get-database path *synchronized-db-name*) nil
      (progn
	(create-database path *synchronized-db-name*)
	(set-synchronized-db path))))

(defun create-non-synchronized-db (path)
  "Creates the non-existing non-synchronized database in the given
directory. Returns t if successfull, nil if not."
  (if (get-database path *non-synchronized-db-name*) nil
      (progn
	(create-database path *non-synchronized-db-name*)
	(set-non-synchronized-db path))))


;;;; MORE PRIVATE CODE

(defun print-storable-info (s sync-data non-sync-data type)
  (format t "~a following storable:
name: ~a
hierarchy: ~a
synchronized data:
~a
non-synchronized data:
~a~%"
	  (cond ((eql type 'store) "Storing")
		((eql type 'load) "Loading")
		(t "Storing/Loading"))
	  (name s) (hierarchy s) sync-data non-sync-data))

(defmethod store2db ((s storable))
  (let ((sync-data (get-synchronized-data s))
	(non-sync-data (get-non-synchronized-data s)))
    (print-storable-info s sync-data non-sync-data 'store)
    ;; TODO: Store data
    ))

(defmethod load-from-db ((s storable))
  (let ((sync-data (list "Title" "Content." 'orange)) ; TODO: Fetch data
	(non-sync-data (list (cons 1 2) (cons 3 4)))) ; TODO: Fetch data
    (print-storable-info s sync-data non-sync-data 'load)
    (set-synchronized-data s sync-data)
    (set-non-synchronized-data s non-sync-data)))

(defmethod get-synchronized-data ((s storable))
  'synchronized-data-from-storable-superclass)

(defmethod get-non-synchronized-data ((s storable))
  'non-synchronized-data-from-storable-superclass)

(defmethod set-synchronized-data ((s storable) data)
  (format t "Nothing to do with the stored synchronized data in
storable superclass: ~%~a~%" data))

(defmethod set-non-synchronized-data ((s storable) data)
  (format t "Nothing to do with the stored non-synchronized data in
storable superclass: ~%~a~%" data))
