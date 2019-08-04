(in-package :cl-user)
(defpackage :mira.core.bootable
  (:use :cl :cl-annot :mira.core.debug))

(annot:enable-annot-syntax)

@export
(defclass <bootable-mixin> () ())

@export
(defgeneric boot (object)
  (:documentation "Initiate startup sequence"))

@export
(defgeneric reboot (object)
  (:documentation "Initiate a restart"))

@export
(defgeneric halt (object)
  (:documentation "Halt the system"))

@export
(defgeneric poweroff (object)
  (:documentation "Force a halt of the system"))
