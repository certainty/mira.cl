(in-package :cl-user)

(defpackage :mira.core.channel
  (:use :cl :cl-annot :mira.core.debug :mira.core.utils :mira.core.bootable :mira.core.message-passing :mira.core.logging)
  (:import-from :mira.core.message
                :<message>))

(in-package :mira.core.channel)
(annot:enable-annot-syntax)

@export
(defclass <channel> (<message-passable>) 
  ((tag      :documentation "Can be used to store abritary data"
	     :initarg :tag
             :initform ""
	     :reader tag)
   (kernel   :documentation "A reference to the bot-kernel that this channel is connected to"
             :initarg :kernel
             :accessor kernel)
   (thread   :documentation "The actual worker thread")
   (id       :documentation "The identifier of this kind of channel"
	     :initarg :id
             :initform (gensym)
	     :reader id)))

(defmethod print-object ((object <channel>) stream)
  (print-unreadable-object (object stream :type t)
   (format stream "id: ~A tag: ~A" (id object) (tag object))))

@export
(defgeneric receive-messages-from-channel (channel)
  (:documentation "Template generic that must be implemented from channel-subclasses. They're expected
to read from whatever input they're bound to and return a list of message-objects. This method is expected to be non-blocking.
"))

@export
(defgeneric write-messages-to-channel (channel messages)
  (:documentation "Template generic that must be implemented from channel subclasses. Implement this to write out
message object to whatever output the specific channel uses"))

@export
(defgeneric generate-thread-name (channel)
  (:documentation "Generate a name for the workchannel")
  (:method ((channel <channel>)) (string (gensym))))

(defmacro in-thread (name &body body)
   `(bordeaux-threads:make-thread
     (lambda () ,@body) :name ,name))

@export
(defmethod boot ((channel <channel>))
  (setf
   (slot-value channel 'thread)
   (in-thread (generate-thread-name channel)
     (message-loop channel))))

@export
(defgeneric message-loop (channel)
  (:documentation "This is the heart of the channel. It repeats two things.
1) reading messages from the kernel and writing them out
2) reading messages from its own imput and sending those to the kernel
  ")
  (:method ((channel <channel>))
    (labels ((exit-loop ()
               (return-from message-loop)))
      (loop
         (handle-kernel-messages
          channel
          (receive-messages-from-kernel channel)
          #'exit-loop)

         (handle-channel-messages
          channel
          (receive-messages-from-channel channel))
       
         ;;give other threads some time 
         (bordeaux-threads:thread-yield)         
         (sleep .2)))))

@export
(defgeneric receive-messages-from-kernel (channel)
  (:documentation "Check if the kernel has messages for us and retrieve them")
  (:method ((self <channel>))
    (pull-messages self)))

(defgeneric handle-kernel-messages (channel messages exit)
  (:documentation "Handle messages that come from the kernel. This messages are most likely answers or messages
to control the system
")
  (:method (channel messages exit)
    (multiple-value-bind (system-messages non-system-messages) (categorize-messages messages)
      (handle-system-messages system-messages exit)
      (write-messages-to-channel channel non-system-messages))))

(defun system-message-p (message)
  "Check if the message is a system message"
  (typep message (find-class 'mira.core.message:<system-message>)))

(defun categorize-messages (messages)
  "Partition a list of messages into system and non-system messages"
  (let ((system-messages
         (remove-if (complement #'system-message-p) messages))
        (rest
         (remove-if   #'system-message-p messages)))
    (values system-messages rest)))

@export
(defgeneric handle-system-messages (messages shutdown)
  (:documentation "Process system-messages that control the overall behavior of the system")
  (:method (messages shutdown)
    (dolist (message messages)
      (with-slots (mira.core.message:data) message
	(log5:log-for (channel debug) "received system message")
        ; simply shutdown if we receive the shutdown message
        (if (eql mira.core.message:data 'shutdown)
            (funcall shutdown))))))

@export
(defmethod halt ((self <channel>))
  (let ((timer (sb-ext:make-timer
                (lambda ()
                  (when (slot-value self 'thread)
                    (bordeaux-threads:destroy-thread (slot-value self 'thread))))))
        (max-seconds 5))
    ;;we give the thread max-seconds to terminate
    (sb-ext:schedule-timer timer max-seconds)
    (bordeaux-threads:join-thread (slot-value self 'thread))
    (setf (slot-value self 'thread) nil)))

@export
(defgeneric handle-channel-messages (channel messages)
  (:documentation "Handle messages that come from the channels input")
  (:method ((channel <channel>) messages)
    (log5:log-for (channel debug) "pushing ~A messages to the kernel" (length messages))
    (push-messages (kernel channel) messages)))

@export
(defgeneric virtual-identity-map (channel data)
  (:documentation "
 Map channel-specific data to a virtual identity. Those identities are then stored in the sender-identity-slot.
 This is used to provide a way to identify users in a channel-specific manner. For irc-channels for example you
 might want to map the hostmask to an identity. The identity is then used in the acl. This way you only
 need to keep one acl with virtual identities and map users channel-dependent to this identities.
"))


