(defvar *bin-directory* "bin/")
(defvar *main-file-name* "src/main.lisp")
(defvar *executable-file-name* "quick-note.exe")

;;; Load program
(load (merge-pathnames "src/main.lisp"))

;;; Create executable and die
(ensure-directories-exist (merge-pathnames "bin/" *load-truename*))
(sb-ext:save-lisp-and-die
 (merge-pathnames "bin/quick-note.exe" *load-truename*)
 :toplevel #'main :executable t)
