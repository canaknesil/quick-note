(in-package #:canaknesil.quick-note-database)

;;;; General purpose database that is used to store s-expressions in
;;;; files residing on a hierarchy of directories.

;;;; DESIGN SPECIFICATION

;;;; A database is represented by a top level directory, and documents
;;;; are files under this top level directory. Sub-directories are
;;;; nested databases. Documents contain s-expressions.

;;;; There can be neither a database nor a document with a name
;;;; starting with dot. These name are reserved.

;;;; In every database folder, a special signiture file named
;;;; ".signiture" is inserted. The content of every signiture file is
;;;; a big integer that will stocastically prove that it is created by
;;;; this database software. It lowers the risk of interpreting other
;;;; directories in the system as databases. It does not provide
;;;; security though.

;;;; The s-expressions in every document file are wrapped by a
;;;; signiture, an integer to prevent writing to files in the system
;;;; that are not documents.

(defvar *database-error-symbol* 'no-error
  "Interface functions set this, in case of error.")

(defun set-error-and-return (sym ret-val)
  (setf *database-error-symbol* sym)
  ret-val)

(defmacro cond-err-ret ((&rest conditions) &body body)
  "Used to simplify checking, error setting, and evaluating return
value inside cond statement."
  `(cond ,@(loop for c in conditions
	      collect `(,(first c) (set-error-and-return
				    ,(second c)
				    ,(third c))))
	 (t ,@body)))

;;; TODO
(defun create-database-directory (path)
  "Creates the directory and the signiture file for a database."
  path)

;;; database-ref structure Should store the directory and name of the
;;; database. This is the database object that will be returned to the
;;; user.

(defun make-database-ref (path)
  path)

;;; document object Should store the directory and name of the
;;; document.


;;;; INTERFACE

;;;; The following comments are not documentation for the interface,
;;;; but the rest of the design specification. Documentations should
;;;; be written for each interface object.

(defun create-database (directory name)
  "Creates and returns a database in 'directory' named 'name'. Sets
the error parameter and returns nil in case of error."
  (let ((db-path (pathname-as-directory
		  (merge-pathnames directory name))))
    (cond-err-ret
     (((not (directory-p directory)) 'no-such-directory nil)
      ((directory-p db-path) 'directory-already-exists nil)
      ((not (create-database-directory db-path)) 'error-creating-directory nil))
     (make-database-ref db-path))))
	


;;; (delete-database directory name) First checks if it is a database
;;; by looking at signiture file. Then changes its name to
;;; ".deleted.0.<old-name>". If a directory with the same name exists
;;; ".deleted.1.<old-name>" is used as name, and so on. It never
;;; actually removes a database directory. Returns t if successfull,
;;; nil if not.

;;; (get-database directory name) Returns the database object. The
;;; directory must exist. In case of error returns nil. This function
;;; just creates an abstract pointer to the pysical database. The
;;; operations on the database must performed via this object. There
;;; is no need for writing the database because the database object
;;; does not contain any information about the content.

;;; (sub-database-list database) Returns the list of nested databases.

;;; (create-sub-database database name)

;;; (get-sub-database database name)

;;; (delete-sub-database database name)

;;; (get-document-list database) Returns the documentes in the
;;; database.

;;; (get-document database name)

;;; (create-document database name s-exp) Creates a document in
;;; 'directory' named 'name' containing 's-exp'. Directory should be a
;;; database. if successfull returns nil, if a document with the same
;;; name exists returns 1, if another error occured returns another
;;; integer. 's-exp' should be wrapped by a container that has the
;;; signiture.

;;; (delete-document database name) First checks the document
;;; signiture and renames if to ".deleted.0.<old-name>". The integer
;;; can be changed if necessary.

;;; (read-document database name)

;;; (update-document database name s-exp)

;;; (database-p database)

;;; (document-p document)

;;; *database-error-symbol* This global variable is used to be set in
;;; case of error. Default value is 'no-error. All the interface
;;; function and macros should set it in case of error.



