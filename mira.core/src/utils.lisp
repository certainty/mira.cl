(in-package :cl-user)
(defpackage :mira.core.utils
  (:use :cl :cl-annot :mira.core.debug))

(in-package :mira.core.utils)
(annot:enable-annot-syntax)

@export
(defmethod ensure-list (data)
  "If data is a list it returns just data, otherwise it returns a list with data as its sole argument"
  (if (listp data) data (list data)))

@export
(defun list->string (ls)
  (if (stringp ls) ls  (format nil "~{~A~^ ~}" ls)))
