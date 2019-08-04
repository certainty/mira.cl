(in-package :cl-user)
(defpackage :mira.processors.echo
  (:use :cl :cl-annot :mira.core.message)
  (:import-from :mira.core.processor
                :<processor>
                :process-message
                :answer))

(in-package :mira.processors.echo)
(annot:enable-annot-syntax)

@export
(defclass <processor-echo> (<processor>)
  ())

@export
(defmethod process-message ((self <processor-echo>) message skip quit)
  (call-next-method)
  (let ((content (mira.core.message:data message)))
    (answer
     message
     (format nil "You said: ~A" content)
     :channels (list "stdio")
     :sender-id 'stdin)))


