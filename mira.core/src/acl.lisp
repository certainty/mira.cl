(in-package :cl-user)
(defpackage :mira.core.acl
  (:use :cl :cl-annot :mira.core.debug))

(in-package :mira.core.acl)
(annot:enable-annot-syntax)

@export
(defclass <acl> ()
  ((level-mapping :initform nil
		  :accessor level-mapping)
   (test :initform #'equal)))

(defmethod initialize-instance :after ((acl <acl>) &rest args)
  (declare (ignore args))
  (setf (level-mapping acl) (make-hash-table :test (slot-value acl 'test))))

@export
(defgeneric access-allowed? (acl ident needed-level)
  (:documentation "Check if access can be granted. It will be granted if the assigned access-level for the given
   identity is >= the needed-level.")
  (:method ((acl <acl>) ident needed-level)
    (>= (access-level acl ident) needed-level)))

@export
(defgeneric access-level (acl ident)
  (:documentation  "Returns the access-level for the given identity. If the identity does not have a level it returns 0")
  (:method ((acl <acl>) ident)
    (gethash ident (level-mapping acl) 0)))

@export
(defgeneric update-access-level (acl ident new-level)
  (:documentation "Sets the access-level for the given identity")
  (:method ((acl <acl>) ident access-level)
    (setf (gethash ident (level-mapping acl)) access-level)))

@export
(defun create-acl-from-alist (alist)
  (let ((acl (make-instance '<acl>)))
    (loop for (ident level) in alist
       do (update-access-level acl ident level))
    acl))

;;create and remove can be constructed with update
;;(update-access-level acl ident nil) => remove
;;(update-access-level acl new-ident level) => create
