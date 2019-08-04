(in-package :cl-user)
(defpackage :mira.core.processor
  (:use :cl :cl-annot :mira.core.debug :mira.core.acl :mira.core.message :getopt)
  (:export id documentation command-environment-issuer command-environment-answer-fn command-environment-skip-fn command-environment-quit-fn command-environment-processor))

(in-package :mira.core.processor)
(annot:enable-annot-syntax)

;; every processor can define commands that
;; are related to that processors. This way
;; the logic to process commands from users
;; can be used by all processors

@export
(defvar *command-prefix* "!")
@export
(defvar *command-acl* (make-instance '<acl>))
@export
(defvar *access-denied-message* "Heh, nice try. You are not allowed to do that, dude!")

@export
(defclass <command-collection> ()
  ((commands :initform (make-hash-table :test #'equal)
             :accessor commands)
   (acl      :initarg :acl
	     :initform *command-acl*
             :accessor acl
             :documentation "The access control list for this commands")))
@export
(defclass <command> ()
  ((usage
    :initarg :usage
    :reader usage
    :initform "")
   (help
    :initarg :help
    :reader help
    :initform "")
   (allow-no-arguments
    :documentation "Can this command be called without arguments?"
    :initarg :allow-no-args
    :reader allow-no-args
    :initform nil)
   (command-line-options
    :documentation "The options to be parsed to getopt"
    :initarg :command-line-options
    :accessor command-line-options
    :initform nil)
   (required-access-level
    :initarg :required-access-level
    :initform 0
    :reader  required-access-level)
   (body
    :initarg :body
    :reader body)))

@export
(defclass <processor> ()
  ((id            :documentation "The identifier of this processor"
		  :initarg :id
		  :initform  "basic-processor"
		  :reader id
		  :allocation :class)
   (kernel        :documentation "The kernel we belong to"
                  :initarg :kernel
		  :accessor kernel)
   (documentation :documentation "Describe what your processor does"
		  :initarg :documentation
		  :initform "No documentation available"
		  :allocation :class)))

@export
(defclass <commands-mixin> ()
  ((self         :documentation "The identity of this bot"
                 :accessor self
                 :initarg :self
                 :initform nil)
   (prefix       :documentation "The command prefix to use"
		 :initarg :prefix
		 :accessor prefix
		 :initform *command-prefix*)
   (commands     :documentation "Commands that are associated with this processor"
		 :accessor commands
                 :initform (make-instance '<command-collection> :acl *command-acl*))))
@export
(defstruct command-environment issuer processor answer-fn skip-fn quit-fn)
  

@export
(defun cmd-answer (env &rest arguments)
  (let ((answer-fn (command-environment-answer-fn env)))
    (apply answer-fn arguments)))

(defun answer (message result &key (channels) (sender-id) (receiver (sender message))
  (messages (mapcar
	     (lambda (data)
	       (make-instance '<message>
			      :receiver receiver
			      :data data
			      :sender-identity sender-id
			      :generator-id (slot-value message 'mira.core.message::generator-id)
			      :tag (tag message))
	       (if (listp result) result (list result)))))))

@export
(defun option-value (value opts &optional (default nil))
  (or (cdr (assoc value opts :test #'equal)) default))
    
@export
(defgeneric process-message (processor message skip-following quit))

(defun make-answer-function (message skip)
  (lambda (result &key (channels) (sender-id) (receiver (sender message)))
    (let ((messages (mapcar
		     (lambda (data)
		       (make-instance '<message>
				      :receiver receiver
				      :data data
				      :sender-identity sender-id
				      :generator-id (slot-value message 'mira.core.message::generator-id)
				      :tag (tag message)))
		       (if (listp result) result (list result)))))
      (funcall skip messages))))

(defmethod  process-message ((processor <commands-mixin>) (message <message>) skip-following quit)
  (let* ((ident (sender-identity message))
	 (input (data message))
	 (self  (string-downcase (self processor)))
	 (prefix (prefix processor))
	 (commands (commands processor))
	 (command-env (make-command-environment :issuer ident 
						:processor processor 
						:answer-fn (make-answer-function message skip-following) 
						:skip-fn skip-following 
						:quit-fn quit)))
    (when (command-syntax? input prefix)
      (multiple-value-bind (command arguments) (destructure-command input prefix)
	(let ((cmd (find-command command commands ident)))
	  (case cmd
	    ((:access-denied) (cmd-answer command-env *access-denied-message*))
	    ((nil) nil)
	    (t (apply-command cmd arguments command-env))))))))

(defgeneric apply-command (command argument-string command-env)
  (:documentation "Applies the command to the given input")
  (:method ((command <command>) argument-string command-env)
    (let* ((argument-list (argument-string->argument-list argument-string))
	   (cmd-closure   (body command))
	   (cmd-opts      (command-line-options command))
	   (first-arg     (car argument-list))
	   (no-args-ok    (allow-no-args command)))
      (if (and first-arg (stringp first-arg) 
	       (or
		(equal (string-downcase (car argument-list)) "--help")
		(equal (string-downcase (car argument-list)) "help")))
          (cmd-answer command-env (help command))
	  (handler-case 
	      (progn		
		(multiple-value-bind (args options errors) (getopt argument-list cmd-opts)
		  (let ((args (delete-if (lambda (e) (equal e "")) args)))
		    (if (or errors (and (not no-args-ok) (not args)))
			(cmd-answer command-env (usage command))
			(apply cmd-closure command-env options args)))))
	    (error ()
	       (format t "Have an error~%")
	       (cmd-answer command-env (usage command))))
	  ))))

(defmethod print-object ((object <processor>) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (id object))))

@export
(defgeneric add-command (proc command-name command)
  (:method ((proc <commands-mixin>) command-name (command <command>))
    (add-command (commands proc) command-name command)))

@export
(defmethod add-command ((collection <command-collection>) name command)
  (with-slots (commands) collection
    (setf (gethash (string-downcase (string name)) commands) command)))

@export
(defmethod get-command ((collection <command-collection>) name)
  (with-slots (commands) collection
    (gethash (string-downcase (string name)) commands)))

@export 
(defgeneric setup-commands (processor)
  (:documentation "Template method to be implemented by every processor that wants to use commands"))


(defmethod initialize-instance :after ((processor <commands-mixin>) &rest args)
  (declare (ignore args))
  (setup-commands processor))


;; (defcommand proc 10 'my-command "A basic command"
;;   '(("option1" :required 5 "This is a required option with default 5")
;;     ("optons2" :none nil "This is an option that doesn't take an argument")
;;     ("option3" :optional 10 "This is an option that optionally takes an argument"))
;;   (lambda (env parsed-options &rest args)
;;     (cmd-answer env (format nil "I received ~A with ~A" parsed-options args)))
  
;;   :no-args-allowed nil
;;   :usage "My-Command: [options] arg1")
@export
(defmethod defcommand ((proc <commands-mixin>) level name descr options handler &key (no-args-allowed nil) (usage))
  (let* ((usage-text (or usage (format nil "Usage: ~A~A [options] [args]" *command-prefix* name)))
	 (help  (generate-help name descr usage-text options)))
    (format t "Options: ~S~%" (options-description->getopt-options options))
    (add-command proc 
		 (string name)
		 (make-instance '<command>
				:required-access-level level
				:help  help
				:command-line-options (options-description->getopt-options options)
				:allow-no-args no-args-allowed
				:usage usage-text
				:body handler))))

(defun options-description->getopt-options (options)
  (mapcar #'butlast options))

(defun command-syntax? (input prefix)
  (cl-ppcre:scan (format nil "^\s*~A(.+?)" (cl-ppcre:quote-meta-chars prefix)) input))

(defgeneric find-command (cmd collection issuer)
  (:documentation "Lookup a command and return if it is found an the issuer has enough privileges. Returns the command or the keyword :access-denied")
  (:method (cmd (collection <command-collection>) issuer)
    (let ((command (get-command collection cmd))
          (acl     (acl collection)))
      (if command
	  (or
	   (and
	    (access-allowed? acl issuer (required-access-level command))
	    command)
	   :access-denied)
	  nil))))

(defun destructure-command (str prefix)
  (let* ((stripped (string-trim '(#\Space #\Tab #\Newline) str))
	 (ws (position #\Space stripped)))
    (if ws
	(values (string-trim prefix (subseq stripped 0 ws))
		(string-trim " " (subseq stripped (1+ ws))))
	(values (string-trim prefix stripped) ""))))

(defun argument-string->argument-list (argument-string)
  (split-sequence:split-sequence #\Space argument-string))

(defun generate-help (command banner usage options)
  (let ((help-string (format nil "~A: ~A" command banner))
	(usage-string usage)
	(options-start "Options: ")
	(options-strings (format-options options)))
    (if options
	(append  (list help-string usage-string options-start) options-strings)
	(list help-string usage-string))))

(defun format-options (options)
  (mapcar
   (lambda (option)
     (let* ((name (first option))
	    (maybe/default (third option))
	    (descr (fourth option)))
       (if maybe/default
	   (format nil "--~A [Default=~A] ~A" name maybe/default descr)
	   (format nil "--~A: ~A " name descr))))
   options))
