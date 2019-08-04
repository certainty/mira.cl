(in-package :cl-user)
(defpackage :mira.core.message-passing
  (:use :cl :cl-annot :mira.core.debug))

(in-package :mira.core.message-passing)
(annot:enable-annot-syntax)

;; This is a mixin to be used for classes that shall support message passing
@export
(defclass <message-passable> ()
  ((mailbox :initform (sb-concurrency:make-mailbox)
            :reader mailbox)))

@export
(defgeneric push-messages (object messages)
  (:documentation "Push messages to the given object")
  (:method ((object <message-passable>) messages)
    (with-slots (mailbox) object
      (dolist (message messages)
        (sb-concurrency:send-message mailbox message)))))

@export
(defgeneric pull-messages (object)
  (:documentation "Pull out any exiting messages and return a list of them.")
  (:method ((object <message-passable>))
    (with-slots (mailbox) object
      (let ((messages (sb-concurrency:receive-pending-messages mailbox)))
	(log5:log-for (debug) "have ~A messages received from the mailbox" (length messages))
        messages))))

@export
(defgeneric pull-one-message (object)
  (:documentation "Retrieve exactly one message no matter how many messages are there. Returns nil if there are no messages")
  (:method ((object <message-passable>))
    (with-slots (mailbox) object
      (sb-concurrency:receive-message mailbox))))
