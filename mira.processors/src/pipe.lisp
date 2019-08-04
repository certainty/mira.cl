(in-package :cl-user)
(defpackage :mira.processors.pipe
  (:use :cl :cl-annot :mira.core.message :mira.core.utils)
  (:import-from :mira.core.processor
                :<processor>
                :process-message))

(in-package :mira.processors.pipe)
(annot:enable-annot-syntax)

@export
(defclass <processor-pipe> (<processor>)
  ((endpoints :documentation "An alist holding endpoint ids"
              :initarg :endpoints
              :initform (list)
              :accessor endpoints)))

;; a specialized class to pipe from non-irc to irc
@export
(defclass <processor-pipe-to-irc> (<processor-pipe>)
  ((irc-destinations
    :documentation "The destination that this pipe shall deliver to"
    :initarg :irc-destinations
    :initform nil)))

(defmethod initialize-instance :after ((processor <processor-pipe>) &rest args)
  (declare (ignore args))
  (setf (slot-value processor 'documentation)
        "A processor that echos the input from one channel to one or more others.
Optionally you may implement the pipe generic method using to do something with the data"))

(defmethod process-message ((self <processor-pipe>) message skip-following quit)
  (declare (ignore skip-following quit))
  (call-next-method)
  (let ((destinations (assoc (mira.core.message:generator-id message) (endpoints self))))
    (when destinations
      (pipe self message destinations))))

@export
(defgeneric pipe (proc message destination-ids)
  (:documentation "Pipe the content from one end to the others")
  (:method ((self <processor-pipe>) message destination-ids)
    (make-instance '<message>
                   :sender-identity (slot-value message 'mira.core.message::sender-identity)
                   :sender (slot-value message 'mira.core.message::sender)
                   :receiver (slot-value message 'mira.core.message::receiver)
                   :generator-id (slot-value self 'mira.core.processor::id)
                   :channels (ensure-list destination-ids)
                   :data (slot-value message 'mira.core.message::data))))


(defmethod pipe ((self <processor-pipe-to-irc>) message destination-ids)
  (mapcar
   (lambda (irc-destination)
     (make-instance '<message>
                    :sender-identity (slot-value message 'mira.core.message::sender-identity)
                    :sender (slot-value message 'mira.core.message::sender)
                    :receiver (slot-value message 'mira.core.message::receiver)
                    :generator-id (slot-value self 'mira.core.processor::id)
                    :channels (ensure-list destination-ids)
                    :tag irc-destination
                    :data (slot-value message 'mira.core.message::data)))
   (slot-value self 'irc-destinations)))
