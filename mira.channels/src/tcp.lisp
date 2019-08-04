(in-package :cl-user)

(defpackage :mira.channels.tcp
  (:use :cl :cl-annot :mira.core.utils :mira.core.message :mira.core.channel :mira.core.logging :anaphora)
  (:import-from :trivial-utf-8
                :utf-8-bytes-to-string))

(in-package :mira.channels.tcp)
(annot:enable-annot-syntax)

@export
(defclass <channel-tcp> (<channel>)
  ((host
    :documentation "The ip to listen on"
    :initform "127.0.0.1"
    :initarg :host
    :reader host)
   (port
    :documentation "Port to listen on"
    :initarg :port
    :reader port
    :initform 6473)
   (clients
    :initform (make-hash-table)
    :accessor clients)
   (reader-thread
    :initform nil)
   (reader-mailbox
    :initform (sb-concurrency:make-mailbox))))

(defclass <tcp-client> ()
  ((buffer
    :accessor buffer
    :initform (make-array 0 :element-type 'unsigned-byte :fill-pointer 0 :adjustable t))))


(defmethod boot ((channel <channel-tcp>))
  (log5:log-for (notice channel) "Booting tcp channel")
  (setf (slot-value channel 'reader-thread)
        (bordeaux-threads:make-thread
         (lambda ()
           (start-reader channel))))
  (call-next-method))

(defgeneric start-reader (channel)
  (:documentation "Starts the tcp-server thread")
  (:method ((channel <channel-tcp>))
    (let* ((master-socket (usocket:socket-listen (host channel) (port channel) :reuse-address t :element-type 'unsigned-byte))
           (sockets (list master-socket))
           (connlock (bordeaux-threads:make-lock)))
      (loop
         (loop :for socket :in (usocket:wait-for-input sockets) :do
            (handler-case
                (if (eq socket master-socket)
                    (progn
                      (bordeaux-threads:with-lock-held (connlock)
                        (unless (null (slot-value socket 'usocket::state))
                          (let ((new-connection (usocket:socket-accept socket)))
                            (setf sockets (push new-connection sockets))
                            (handle-client-connect channel new-connection)))))
                    (when (listen (usocket:socket-stream socket))
                      ;;handle client data
                      (when (handle-client-input channel socket)
                        ;;all data read
                        (bordeaux-threads:with-lock-held (connlock)
                          (handle-client-disconnect channel socket)
                          (setf sockets (delete socket sockets))
                          (usocket:socket-close socket)))))
              (end-of-file () )))))))

(defgeneric handle-client-connect (channel new-connection)
  (:documentation "Do something when a new client connects")
  (:method ((channel <channel-tcp>) new-connection)
    (log5:log-for (notice channel) "tcp connect")
    (with-slots (clients) channel
      (setf (gethash new-connection clients)
            (make-instance '<tcp-client>)))))

(defgeneric handle-client-input (channel socket)
  (:documentation "Do something with the client input")
  (:method ((channel <channel-tcp>) socket)
    (log5:log-for (notice channel) "Handing client input")
    (let ((mb (slot-value channel 'reader-mailbox)))
      (with-slots (clients) channel
        (let ((client (gethash socket clients)))
          (awhen (client-read client socket)
            (log5:log-for (debug channel) "Adding message to queue")
            (sb-concurrency:send-message mb it)
            t))))))

(defgeneric handle-client-disconnect (channel socket)
  (:method ((channel <channel-tcp>) socket)
    (log5:log-for (notice channel) "tcp disconnect")
    (with-slots (clients) channel
      (remhash socket clients))))

(defgeneric client-read (client socket)
  (:method ((client <tcp-client>) socket)
    (with-slots (buffer) client
      (when (collect-input socket buffer)
        (prog1
            (string (utf-8-bytes-to-string buffer))
          (reset-buffer client))))))

(defun reset-buffer (client)
  (setf (fill-pointer (buffer client)) 0))

(defun collect-input (socket buffer &optional (end-char 10))
  (loop
     :with stream = (usocket:socket-stream socket)
     :with byte
     :while (listen stream)
     :doing
     (setq byte (read-byte stream))
     (when (= byte end-char)
       (return t))
     (vector-push-extend byte buffer)))

@export
(defmethod halt ((channel <channel-tcp>))
  (declare (ignore args))
  t)

@export
(defmethod receive-messages-from-channel ((channel <channel-tcp>))
  (let ((messages (sb-concurrency:receive-pending-messages (slot-value channel 'reader-mailbox))))
    (when messages
      (mapcar
       (lambda (data)
         (log5:log-for (notice channel) (format nil "Have input from tcp: ~S" data))
         (make-instance '<message>
                        :sender-identity "tcp"
                        :sender "tcp"
                        :generator-id (slot-value channel 'mira.core.channel::id)
                        :data data))
       messages))))

(defmethod write-messages-to-channel ((channel <channel-tcp>) messages)
  (declare (ignore channel messages)))

