(in-package :cl-user)

(defpackage :mira.core.logging
  (:use :cl)
  (:import-from :log5
                :defcategory
                :start-sender
                :stream-sender)
  (:export kernel processor channel message security error warning info notice debug setup-logging *log-stream*))

(in-package :mira.core.logging)
(annot:enable-annot-syntax)

(defcategory kernel)
(defcategory processor)
(defcategory channel)
(defcategory message)
(defcategory security)
(defcategory error)
(defcategory warning)
(defcategory info)
(defcategory notice)
(defcategory debug)

(defcategory warning+ (or warning error))
(defcategory all (or kernel processor message security debug notice info warning error))

@export 
(defparameter *log-stream* nil)

@export 
(defun setup-logging ()
  "By default we start a logger that logs to stdout"
  (start-sender 'application
                (stream-sender :location (or *log-stream* *standard-output*))
                :output-spec '(time log5:category log5:message)
                :category-spec '(all)))

