(in-package :cl-user)
(defpackage :mira.core.debug
  (:use :cl :cl-annot :mira.core.logging))


(in-package :mira.core.debug)
(annot:enable-annot-syntax)

@export
(defparameter *debug* nil)

@export
(defun dbg (fmt &rest arguments)
  (when *debug*
    (let ((msg (apply #'format nil fmt arguments)))
      (log5:log-for (debug) msg)
      (format t "[DEBUG]: ")
      (format t msg)
      (format t "~%"))))
