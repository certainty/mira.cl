(in-package :cl-user)

(defpackage :mira.channels.stdio
  (:use :cl :cl-annot :mira.core.utils :mira.core.channel)
  (:import-from :mira.core.message
                :<message>
                :data))

(in-package :mira.channels.stdio)
(annot:enable-annot-syntax)

@export
(defclass <channel-stdio> (<channel>) ())

(defun line-to-message (data id)
  (make-instance '<message>
                 :sender-identity nil
                 :sender "stdin"
                 :generator-id id
                 :data data))

@export
(defmethod receive-messages-from-channel ((channel <channel-stdio>))
  (ensure-list
   (and (listen *standard-input*)
        (line-to-message (read-line *standard-input*) (slot-value channel 'mira.core.channel::id)))))

@export
(defmethod write-messages-to-channel ((channel <channel-stdio>) messages)
  (dolist (message messages)
    (format t "~%>> ~A~%~A> " (data message) "MIRA ")
    (force-output)))

@export
(defmethod message-loop :before ((channel <channel-stdio>))
  "Just start the message loop"
  (format t "~%~%MIRA > ")
  (force-output))

@export
(defmethod generate-thread-name ((channel <channel-stdio>))
  "STDIO-CHANNEL")
