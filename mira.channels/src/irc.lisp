(in-package :cl-user)
(defpackage :mira.channels.irc
  (:use :cl :cl-annot :mira.core.debug :mira.core.utils :mira.core.channel :mira.core.logging)
  (:import-from :mira.core.message
                :<message>
                :data))

(in-package :mira.channels.irc)
(annot:enable-annot-syntax)

@export
(defclass <channel-irc> (<channel>)
  ((channels :initarg :channels
             :initform (list))
   (nick     :initarg :nick
             :initform (string (gensym)))
   (cl-irc-connect-arguments
    :initarg :cl-irc-connect-arguments
    :initform (list))
   (connection
    :initform nil
    :accessor irc-connection)))

(define-condition lost-connection-error (error)
  ((message :initarg error-message
            :accessor error-message
            :initform nil)))

(defmethod print-object ((object lost-connection-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (error-message object))))

(defun signal-lost-connection (message)
  (error 'lost-connection-error :error-message message))

(defmacro with-valid-connection ((connection) channel &body body)
  `(let ((,connection (irc-connection ,channel)))
    (unless (cl-irc::connectedp ,connection)
      (signal-lost-connection "Lost connection to the irc"))
    (progn ,@body)))

(defmacro with-standard-restarts (channel continuation &body body)
  `(restart-case ,@body
     (reconnect ()
       :report (lambda (stream) (format stream "Reconnect to irc and retry"))
       (funcall #'reconnect-to-irc ,channel ,continuation))
     (skip () nil)))

(defmacro reconnecting-if-needed (&body body)
  `(handler-bind
       ((lost-connection-error (lambda (c)
                                 (let ((restart (find-restart 'reconnect)))
                                   (when restart (invoke-restart restart))))))
     ,@body))

(defgeneric reconnect-to-irc (channel continuation)
  (:method ((channel <channel-irc>) continuation)
    (ignore-errors
      (cl-irc:quit (irc-connection channel)))
    (log5:log-for (mira.core.logging:channel mira.core.logging:warning) "Reconnecting to irc in 5 seconds~%")
    (sleep 5)
    (connect-to-irc channel)
    (sleep 2)
    (join-channels channel)
    (funcall continuation)))

(defgeneric end-of-names-p (message)
  (:documentation "Have we found the end-of-names server message")
  (:method (message)   nil))

(defmethod end-of-names-p ((message cl-irc::irc-rpl_endofnames-message)) t)

(defgeneric read-initial-input (channel)
  (:documentation "slurp in the initial input from the server that is not of interest for us. Some irc server allow commands only
after a given server-message. So we wait until we see that")
  (:method ((channel <channel-irc>))
    (let ((internal-connection (irc-connection channel)))
      (loop while
           (if (listen (cl-irc:network-stream internal-connection))
               (let ((msg (cl-irc:read-message internal-connection)))
                 (if (end-of-names-p msg) nil t)))))))

(defgeneric join-channels (channel)
  (:method ((channel <channel-irc>))
    (dolist (irc-channel (slot-value channel 'channels))
      (log5:log-for (mira.core.logging:channel mira.core.logging:notice) (format nil "Joining channel ~A~%" irc-channel))
      (cl-irc:join (irc-connection channel) irc-channel))))

(defmethod initialize-instance :after ((channel <channel-irc>) &rest args)
  (declare (ignore args))
  (connect-to-irc channel)
  (sleep 2)
  (join-channels channel))

(defun privmsg-p (message)
  (eql 'CL-IRC:IRC-PRIVMSG-MESSAGE (class-name (class-of message))))

(defun ping-p (message)
  (eql 'CL-IRC:IRC-PING-MESSAGE (class-name (class-of message))))

(defun pong!  (con server &optional (server2 ""))
  (when (cl-irc::connectedp con)
    (cl-irc::send-irc-message con :pong server server2)))

(defun data-available-p (channel)
  (let ((internal-connection (irc-connection channel)))
    (and (cl-irc::connectedp internal-connection)
         (listen (cl-irc:network-stream internal-connection)))))

(defun read-message-from-irc (channel)
  "Read data from the channel"
  (with-valid-connection (connection) channel
    (when (data-available-p channel)                     
      (cl-irc:read-message connection))))

(defun write-message-to-irc(channel receiver message)
  "Write data to the irc-channel"
  (with-valid-connection (connection) channel                 
    (cl-irc:privmsg connection receiver message)))

(defmethod receive-messages-from-channel ((channel <channel-irc>))
  (ensure-list
   (let ((irc-message (reconnecting-if-needed
                       (with-standard-restarts channel (lambda () (receive-messages-from-channel channel))
                         (read-message-from-irc channel)))))
     (when irc-message
       (let ((full-source (concatenate 'string (cl-irc:user irc-message) "!" (cl-irc:host irc-message))))
         (cond
           ((ping-p irc-message)
            (pong! (cl-irc::connection irc-message) (cl-irc::arguments irc-message)) nil)
           ((privmsg-p irc-message)
            (create-mira-message channel irc-message full-source))
           (t nil)))))))

(defgeneric connect-to-irc (channel)
  (:documentation "Reconnect to the irc-endpoint")
  (:method ((channel <channel-irc>))
    (with-slots (connection) channel
      (let ((new-connection (apply #'cl-irc:connect (slot-value channel 'cl-irc-connect-arguments))))
        (if (cl-irc::connectedp new-connection)
            (setf connection new-connection)
          (read-initial-input channel))))))
           
(defun self-p (channel name)
  (string-equal name (slot-value channel 'nick)))

(defun create-mira-message (channel irc-message full-source)
  (let* ((sender-nick (cl-irc:source irc-message))
         (data (cl-irc:arguments irc-message))
         (receiver (first data))
         (receiver-in-response (if (self-p channel (first data)) sender-nick (first data))))
    (make-instance '<message>
                   :sender-identity (virtual-identity-map channel (list full-source sender-nick))
                   :sender sender-nick 
                   :receiver receiver 
                   :data (second data)
                   :generator-id (slot-value channel 'mira.core.channel::id)
                   :tag receiver-in-response)))

(defmethod write-messages-to-channel ((channel <channel-irc>) messages)
  (reconnecting-if-needed 
   (with-standard-restarts channel (lambda () (write-messages-to-channel channel messages))
     (let ((internal-connection (irc-connection channel)))
       (dolist (message messages)
         (let ((data-with-nick (concatenate 'string (mira.core.message::receiver message) ": " (mira.core.message::data message))))
           ;;the destination is stored in the tag rather than 
           ;;the receiver. All processors send messages to the source of a message
           ;;which is not what we want for irc. since the source is allways the nick
           ;;but we may want to send the response to the channel or to the nick
           (format t "Writing data to irc~%")
           (write-message-to-irc channel (mira.core.message::tag message) data-with-nick)))))))
@export
(defmethod halt ((channel <channel-irc>))
  (let ((int-con (irc-connection channel)))
    (cl-irc:quit int-con)
    (call-next-method)))

