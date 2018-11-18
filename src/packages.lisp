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
   :create-database
   :get-database-error
   :get-database
   :delete-database))
