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
