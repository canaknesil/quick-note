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
(defvar *database-sig-file-name* ".signiture")
(defvar *database-sig* 534427522720282064455979
  "Random number with 24 digits")
(defvar *document-sig* *database-sig*)


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

(defun get-sig-file-path (path)
  (merge-pathnames path *database-sig-file-name*))

(defun create-database-sig (path)
  "Returns t if success, returns nil the file already exists, generate
error if directory does not exist."
  (with-open-file (file (get-sig-file-path path)
			:direction :output
			:if-exists nil)
    (if (not file) nil
	(progn
	  (print *database-sig* file)
	  t))))

(defun create-database-directory (path)
  "Creates the directory and the signiture file for a database."
  (cond
    ((not (ensure-directories-exist path)) nil)
    ((not (create-database-sig path)) nil)
    (t t)))

(defun database-directory-p (path)
  "Checks if the directory is a database."
  (with-open-file (file (get-sig-file-path path)
			:direction :input
			:if-does-not-exist nil)
    (if (not file) nil
	(eql *database-sig* (read file)))))

(defun create-database-path (directory name)
  (pathname-as-directory
   (merge-pathnames name directory)))

(defun deleted-database-path (path n)
  "Returns the renamed path."
  (pathname-as-directory
   (merge-pathnames
    (concatenate 'string
		 ".deleted."
		 (car (last (pathname-directory path)))
		 "."
		 (write-to-string n))
    (pathname-as-file path))))

(defun deleted-document-path (path n)
  "Returns the renamed path."
  (merge-pathnames
   (concatenate 'string
		".deleted."
		(pathname-name path)
		(let ((type (pathname-type path)))
		  (if type (concatenate 'string "." type) ""))
		"."
		(write-to-string n))
   path))

(defun delete-database-with-n (old-path n)
  "Renames the database top directory with the first available number
starting from n."
  (let ((new-path (deleted-database-path old-path n)))
    (if (directory-p new-path)
	(delete-database-with-n old-path (1+ n))
	(rename-file (pathname-as-file old-path)
		     (merge-pathnames (pathname-as-file new-path))))))

(defun delete-document-with-n (old-path n)
  (let ((new-path (deleted-document-path old-path n)))
    (if (probe-file new-path)
	(delete-database-with-n old-path (1+ n))
	(rename-file old-path (merge-pathnames new-path)))))

(defun write-document-content (value stream)
  (print *document-sig* stream)
  (print value stream)
  (print 'kebab-tallrik stream)
  value)

;;; database-ref structure. Should store the directory and name of the
;;; database. This is the database object that will be returned to the
;;; user.

(defun make-database-ref (path)
  path)

(defun get-database-ref-path (database)
  database)

(defun get-database-ref-name (database)
  (car (last (pathname-directory
	 (get-database-ref-path database)))))

;;; document object. Should store the directory and name of the
;;; document.

(defun make-document-ref (path)
  path)

(defun get-document-ref-path (document)
  document)

(defun get-document-ref-name (document)
  (let ((path (get-document-ref-path document)))
    (if (eql (pathname-type path) nil)
	(pathname-name path)
	(concatenate 'string
		     (pathname-name path) "."
		     (pathname-type path)))))


;;;; INTERFACE

(defun get-database-error ()
  "Returns the last occured error. To be called after an interface
function returned an error value."
  *database-error-symbol*)

;;; create-database and get-database functions are only two function
;;; that are used to interract with physical file system. Other
;;; functions uses the database instances returned by these
;;; functions. These two functions should check for any erronous
;;; situation so that other function can manage databases without the
;;; need of do all the checks.
(defun create-database (directory name)
  "Creates and returns a database in 'directory' named 'name'. Sets
the error parameter and returns nil in case of error."
  (let ((db-path (create-database-path directory name)))
    (cond-err-ret
	(((not (directory-p (pathname-as-directory directory)))
	  'no-such-directory nil)
	 ((char= (aref name 0) #\.) 'database-name-starting-with-dot nil)
	 ((directory-p db-path) 'directory-already-exists nil)
	 ((not (create-database-directory db-path))
	  'error-creating-directory nil))
	(make-database-ref db-path))))

(defun get-database (directory name)
  "Returns the database object for existing database. Can be also used
to check if the database exists. Sets the error parameter and returns
nil in case of error."
  (let ((db-path (create-database-path directory name)))
    (cond-err-ret
	(((not (directory-p (pathname-as-directory directory)))
	  'no-such-directory nil)
	 ((char= (aref name 0) #\.) 'database-name-starting-with-dot nil)
	 ((not (directory-p db-path)) 'directory-does-not-exists nil)
	 ((not (database-directory-p db-path))
	  'not-a-database nil))
      (make-database-ref db-path))))

(defun delete-database (database)
  "Deletes the database by not removing it but renaming it with a
special name starting with a dot."
  (delete-database-with-n (get-database-ref-path database) 0))

(defun get-database-name (database)
  "Returns the name of the database."
  (get-database-ref-name database))


(defun create-sub-database (database sub-db-name)
  "Creates and returns the sub-database in 'directory' named 'name'. Sets
the error parameter and returns nil in case of error."
  (create-database (get-database-ref-path database)
		   sub-db-name))

(defun get-sub-database (database sub-db-name)
  "Returns the database object for existing sub-database. Can be also
used to check if the database exists. Sets the error parameter and
returns nil in case of error."
  (get-database (get-database-ref-path database)
		sub-db-name))

;;; (delete-sub-database database name)
(defun delete-sub-database (database)
  (delete-database database))

;;; This function explicitly checks the name of the database and does
;;; not include names starting with a dot.
(defun sub-database-list (database)
  "Returns the list of nested databases."
  (mapcar
   #'(lambda (p) (make-database-ref p))
   (remove-if
    #'(lambda (p)
	(or
	 (let ((name (car (last (pathname-directory p)))))
	  (char= (aref name 0) #\.))
	 (not (database-directory-p p))
	 (not (directory-p p))))
    (directory
     (make-pathname :defaults (get-database-ref-path database)
		    :name :wild :type :wild)))))


;;; create-document and get-document functions are only two function
;;; that are used to interract with physical file system for
;;; documents. Other functions uses the document instances returned by
;;; these functions. These two functions should check for any erronous
;;; situation so that other function can manage databases without the
;;; need of do all the checks.
(defun create-document (database name value)
  "Creates a document with value stored in it. If the document already
exists, does not recreate it, sets the error and returns nil."
  (let ((file-path (merge-pathnames name
				    (get-database-ref-path database))))
    (cond-err-ret
	(((char= (aref name 0) #\.) 'document-name-starting-with-dot nil)
	 ((probe-file file-path) 'document-already-exists nil))
      (with-open-file (file file-path :direction :output)
	(write-document-content value file)
      (make-document-ref file-path)))))
	
(defun get-document (database name)
  "Returns the document object for existing document. Can be also used
to check if the document exists. Sets the error parameter and returns
nil in case of error."
  (let ((file-path (merge-pathnames name
				    (get-database-ref-path database))))
    (cond-err-ret
	(((char= (aref name 0) #\.) 'document-name-starting-with-dot nil)
	 ((not (probe-file file-path)) 'document-does-not-exist nil))
      (let ((signiture (with-open-file (file file-path :direction :input)
			 (read file))))
	(cond-err-ret
	    (((not (eql signiture *document-sig*)) 'not-a-document nil))
	  (make-document-ref file-path))))))

(defun read-document (document)
  "Returns the content of the document."
  (with-open-file (file (get-document-ref-path document) :direction :input)
    (read file) ; skip signiture
    (read file)))

(defun update-document (document value)
  (with-open-file (file (get-document-ref-path document)
			:direction :output
			:if-exists :supersede)
    (write-document-content value file)))

(defun delete-document (document)
  "Deletes the document by not removing it but renaming it with a
special name starting with a dot."
  (delete-document-with-n (get-document-ref-path document) 0))

;;; (document-list database) Returns the documentes in the
;;; database.









