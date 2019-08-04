(in-package :cl-user)
(defpackage :mira.core.message
  (:use :cl :cl-annot :mira.core.debug)
  (:export data sender sender-identity tag destination-channels generator-id))

(in-package :mira.core.message)
(annot:enable-annot-syntax)

@export
(defclass <message> ()
  ((tag
    :documentation "Store abritary maybe channel-specific data"
    :initarg :tag
    :initform ""
    :accessor tag)
   (sender-identity
    :documentation "The identity behind the sender. This is used to uniqly and securely identify the sender"
    :initarg :sender-identity
    :accessor sender-identity
    :initform (error "You must supply the identity"))
   (sender
    :documentation "The sender of the message"
    :initarg :sender
    :initform ""
    :accessor sender)
   (receiver
    :documentation "For outgoing messages. This is the target"
    :initarg :receiver
    :initform ""
    :accessor receiver)
   (destination-channels
    :documentation "The list of channel-ids this messages shall be dispatched to"
    :initarg :channels
    :reader destination-channels
    :initform (list))
   (timestamp
    :documentation "The timestamp stating when the message has been created"
    :initarg :timestamp
    :initform (get-universal-time)
    :reader timestamp)
   (generator-id
    :documentation "The id of the component that generated this message"
    :initarg :generator-id
    :initform (error "You must supply the generator id")
    :reader generator-id)
   (process-by
    :documentation "Only send this message to specific processors"
    :initform (list)
    :initarg :process-by
    :reader process-by)
   (data
    :documentation "The data that this message transports"
    :initarg :data
    :initform (error "data must be supplied")
    :accessor data)))

@export
(defclass <system-message> ()
  ((type  :documentation "The type of the system message"
          :initarg :type
          :initform 'system)
   (data  :documentation "The data to transmit"
          :initarg :data
          :initform (error "You must supply data")
          :accessor data)
   (channel-id :documentation "The id of the channel that has send this message"
               :initarg :channel-id)))


(defmethod print-object ((object <message>) stream)
 (print-unreadable-object (object stream :type t)
   (format stream "Sender: ~A Receiver: ~A Generator: ~A  Destinations: ~A Data: ~A"
           (string (sender-identity object))
           (string (receiver object))
           (string (generator-id object))
           (destination-channels object)
           (data object))))

(defmethod print-object ((object <system-message>) stream)
 (print-unreadable-object (object stream :type t)
   (with-slots (type channel-id data) object
     (format stream "type: ~A channel-id: ~A data: ~A " (or type 'unknown) (or channel-id 'unknown)  data))))
