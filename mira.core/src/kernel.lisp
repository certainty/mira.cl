(in-package :cl-user)
(defpackage :mira.core.kernel
  (:use :cl :mira.core.debug :mira.core.utils :mira.core.message-passing :mira.core.bootable :mira.core.logging :mira.core.message)
  (:import-from  :mira.core.processor
                 :<processor>
                 :process-message)
  (:import-from :mira.core.channel
                :boot
                :halt))

(in-package :mira.core.kernel)
(annot:enable-annot-syntax)

@export
(defclass <kernel> (<message-passable>)
  ((channels  
    :documentation "The list of bound channels"
    :initarg :channels
    :initform (list)
    :accessor channels)
   (channels-mutex
    :initform (bordeaux-threads:make-lock))
   (specific-handlers  
    :documentation "A mapping from processors to channels. They're bound to"
    :initarg :specific-handlers
    :accessor specific-handlers
    :initform (make-hash-table :test #'equal))
   (secific-handlers-mutex
    :initform (bordeaux-threads:make-lock))
   (generic-handlers 
    :documentation "list of processors that apply to all channels"
    :initarg :generic-handlers
    :accessor generic-handlers
    :initform (list))
   (generic-handlers-mutex
    :initform (bordeaux-threads:make-lock))))


;; ;(defmethod print-object ((object <kernel>) stream)
;; (print-unreadable-object (object stream :type t)
;;   .  (format stream "")
;;     (let ((spec-procs (mapcar (lambda (o) (slot-value o 'id)) (loop for v being the hash-values in (specific-handlers object) collect v)))
;; 	  (gen-procs (mapcar  (lambda (o) (slot-value o 'id)) (generic-handlers object))))
;;       (format stream "channels: ~{~a~^ ~}~% spec-procs: ~{~a~^ ~}~% gen-procs: ~{~a~^ ~}~%" (mapcar  (lambda (o) (slot-value o 'id)) (channels object)) spec-procs gen-procs))))



@export
(defgeneric bind-channels (kernel new-channels)
  (:documentation "Add a channel to the kernel. The channel will be started by the kernel on startup")
  (:method ((self <kernel>) new-channels)
    (with-slots (channels channels-mutex) self
      (bordeaux-threads:with-lock-held (channels-mutex)
        (dolist (channel new-channels)
          (log5:log-for (notice mira.core.logging::kernel) "Binding channel ~A" (mira.core.channel::id channel))
          (setf (mira.core.channel::kernel channel) self)
          (setf channels (cons channel channels)))))))

@export
(defgeneric prepend-processor (kernel processor &rest args)
  (:documentation "Prepend the processor")
  (:method ((kernel <kernel>) (processor <processor>) &key channel-id)
    (if channel-id
        (with-slots (specific-handlers-mutex) kernel
          (bordeaux-threads:with-lock-held (specific-handlers-mutex)
            (log5:log-for (info mira.core.logging::kernel) "Prepending processor: ~A to channel: ~A" (slot-value processor 'mira.core.processor::id) channel-id)
	    (setf (mira.core.processor::kernel processor) kernel)
            (push processor (gethash channel-id (specific-handlers kernel) (list)))))
        (with-slots (generic-handlers-mutex) kernel
          (bordeaux-threads:with-lock-held (generic-handlers-mutex)
            (log5:log-for (info mira.core.logging::kernel) "Prepending processor: ~A for all channels" (slot-value processor 'mira.core.processor::id))
	    (setf (mira.core.processor::kernel processor) kernel)
            (push processor (generic-handlers kernel)))))))

@export
(defgeneric append-processor (kernel processor &rest args)
  (:method ((kernel <kernel>) (processor <processor>) &key channel-id)
    (if channel-id
        (with-slots (specific-handlers-mutex) kernel
            (bordeaux-threads:with-lock-held (specific-handlers-mutex)
              (let ((ht (specific-handlers kernel)))
                (log5:log-for (info mira.core.logging::kernel) "Appending processor: ~A to channel: ~A" (slot-value processor 'mira.core.processor::id) channel-id)
		(setf (mira.core.processor::kernel processor) kernel)
                (setf (gethash channel-id ht)
                      (append (gethash channel-id ht (list)) (ensure-list processor))))))
        (with-slots (generic-handlers-mutex) kernel
          (bordeaux-threads:with-lock-held (generic-handlers-mutex)
            (log5:log-for (info kernel) "Appending processor: ~A for all channels" (slot-value processor 'mira.core.processor::id))
	    (setf (mira.core.processor::kernel processor) kernel)
            (setf (generic-handlers kernel) (append (generic-handlers kernel) (ensure-list processor))))))))


;; TODO implement specific handlers
(defgeneric apply-processors (kernel messages stop-processing)
  (:documentation "Apply all processors to the messages and generate answers if apropriate")
  (:method ((kernel <kernel>) messages stop-processing)
    (labels ((skip (answers)
	       (log5:log-for (debug mira.core.logging::kernel) "Skipping following messages")
	       (when answers
		 (dolist (answer (ensure-list answers))
		   (write-answer-to-channels kernel answer)))
		 (return-from apply-processors)))
      (log5:log-for (info kernel) "messages received ~A" messages)
      (dolist (message messages)
        (dolist (processor (generic-handlers kernel))
	  (log5:log-for (debug mira.core.logging::kernel) "running processor ~A" (mira.core.processor::id processor))
          (let ((answers (ensure-list (process-message processor message #'skip stop-processing))))
	    (log5:log-for (debug mira.core.logging::kernel) "answers are ~A" answers)
            (when answers
              (dolist (answer answers)
                (write-answer-to-channels kernel answer)))))))))

(defun find-destination-channels (message available-channels)
  (let ((destination-ids
         (or (slot-value message 'mira.core.message:destination-channels)
             (list (slot-value message 'mira.core.message:generator-id)))))

    (if destination-ids
        (remove-if-not
         (lambda (channel)
           (let ((channel-id (slot-value channel 'mira.core.channel::id)))
             (find (string channel-id) destination-ids :test #'string=)))
         available-channels))))

@export
(defgeneric write-answer-to-channels (kernel answer)
  (:documentation "Send the answer to all or selected channels of the kernel")
  (:method ((kernel <kernel>) (message <message>))
    (dolist (channel (find-destination-channels message (channels kernel)))
      (log5:log-for (debug mira.core.logging::kernel) "pushing messages to channel")
      (push-messages channel (list message)))))


@export
(defgeneric pump (kernel)
  (:documentation "Start the main message-loop of the kernel")
  (:method ((kernel <kernel>))
    (labels ((stop-kernel ()
               (halt kernel)
               (return-from pump)))
      (loop
	 (sleep .2)
         (apply-processors
          kernel
          (pull-messages kernel)
          #'stop-kernel)))))

@export
(defmethod halt ((kernel <kernel>))
  (declare (ignore args))
  (log5:log-for (mira.core.logging::kernel info) "Halting the system")
  (dolist (channel (channels kernel))
    (mira.core.channel:halt channel)))

@export
(defmethod boot ((kernel <kernel>))
  (setup-logging)
  (log5:log-for (kernel info) "Booting the system")
  (dolist (channel (channels kernel))
    (log5:log-for (mira.core.logging::kernel notice) "Booting channel ~A" (mira.core.channel::id channel))
    (boot channel))
  (log5:log-for (mira.core.logging::kernel notice) "Booted successfully")
  (pump kernel))
