(in-package :cl-user)

(defpackage :com.gigamonkeys.pathnames
  (:use :common-lisp)
  (:export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p))

(defpackage :canaknesil.quick-note-database
  (:use :common-lisp :com.gigamonkeys.pathnames)
  (:export
   :get-database-error
   :create-database
   :get-database
   :delete-database
   :get-database-name
   :create-sub-database
   :get-sub-database
   :delete-sub-database
   :sub-database-list
   :create-document
   :get-document
   :read-document
   :update-document
   :delete-document
   :document-list))

(defpackage :canaknesil.quick-note-db-controller
  (:use :common-lisp
	:com.gigamonkeys.pathnames
	:canaknesil.quick-note-database)
  (:export
   :storable
   :store2db
   :load-from-db
   :get-synchronized-data
   :get-non-synchronized-data
   :set-synchronized-data
   :set-non-synchronized-data))

(defpackage :canaknesil.quick-note-model
  (:use :common-lisp
	:canaknesil.quick-note-db-controller)
  (:export
   :note))

(defpackage :canaknesil.quick-note-ipc-back-end
  (:use :common-lisp)
  (:export
   :to-do-ipc-back-end))

(defpackage :canaknesil.quick-note-controller
  (:use :common-lisp
	:canaknesil.quick-note-model
	:canaknesil.quick-note-ipc-back-end)
  (:export
   :to-do-controller))
