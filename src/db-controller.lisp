(in-package :canaknesil.quick-note-db-controller)

;;;; Part of the 'model' in MVC design.

;;;; DESIGN SPECIFICATION

;;;; Database controller for quick-note. Uses database.lisp.

;;;; Implements the database operations required by quick-note
;;;; model. Controles two databases, one ment to synchronized between
;;;; computers, other ment to be local. Abstracts the fact that there
;;;; is two seperate databases and provides a simple interface.

