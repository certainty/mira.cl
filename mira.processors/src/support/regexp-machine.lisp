(in-package :cl-user)

(defpackage mira.processors.support.regexp-machine
  (:use :cl :cl-annot :mira.core.debug))

(in-package :mira.processors.support.regexp-machine)
(annot:enable-annot-syntax)

(defvar *rules*  (list) "The list of rules loaded")
(defvar *synonyms* (make-hash-table :test #'equalp))
(defvar *memory* (make-hash-table) "The memory of the bot instance. You can use it to store information. It's a simple key-value store")
(defparameter *rule-environment* (make-hash-table) "A temporary key-value store that represents the current rule-environment. It holds the captures and can hold any temporary knowledge")

(defparameter *current-rule* nil)
(defparameter *fenders* (make-hash-table) "A hashtable that holds named regexps that can be used as fenders for rules")


(defun dump-hash-table (ht &optional (stream t))
  (loop for k being the hash-keys in ht using (hash-value v)
     do
       (format stream "~A -> ~A~%" k v)))

@export
(defun reset-rules ()
  (setf *rules* nil))

@export
(defun reset-memory ()
  (setf *memory* (make-hash-table)))

@export
(defun reset-synonyms ()
  (setf *synonyms* (make-hash-table :test #'equalp)))


@export
(defun % (key &optional (default nil))
  "Retrieve a value from the bot's memory. This is used to hold long term stuff"
  (gethash key *memory* default))

@export
(defun remember (key value)
  "Remember a simple fact"
  (setf (gethash key *memory*) value))

@export
(defun $ (key &optional (default nil))
  "Retrieve a value from the current rule-environment. For example to access captures from the input"
  (gethash key *rule-environment* default))

@export
(defun load-rulebook (path &key (reset-db nil))
  (when reset-db (reset-rules))
  (let ((load-path
         (if  (symbolp path)
              (let ((base-path (asdf:system-source-directory :mira.processors)))
                (format nil "~A/src/support/rules/~A-rules.lisp" base-path (string-downcase (symbol-name path))))
              path)))

    (dbg "Loadining rulebook: ~A~%" load-path)
    (load load-path)))

@export
(defclass <fender> ()
  ((regexp :initarg :regexp
           :reader regexp
           :initform (error "You must supply the regexp"))
   (name   :initarg :name
           :reader name
           :initform (error "You must supply the name"))))

(defmethod initialize-instance :after ((fender <fender>) &rest args)
  (declare (ignore args))
  (setf (slot-value fender 'regexp) (cl-ppcre:create-scanner (regexp fender))))

@export
(defclass <input-scanner> ()
  ((regexp   :initarg :regexp
             :reader regexp
             :initform (error "You must supply the regexp"))
   (regexp-string :reader regexp-string)
   (captures :initarg :captures
             :reader captures
             :initform (list))))

(defmethod print-object ((object <input-scanner>) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (regexp-string captures) object
        (format stream "rx: ~A captures: ~A" regexp-string captures))))

(defmethod initialize-instance :after ((input <input-scanner>) &rest args)
  (declare (ignore args))
  (setf (slot-value input 'regexp-string) (regexp input))
  (setf (slot-value input 'regexp) (cl-ppcre:create-scanner (regexp input))))

@export
(defclass <rule> ()
  ((name   :initarg :name
           :reader name
           :initform (gensym))
   (weight :initarg :weight
           :reader weight
           :initform 0
           :documentation "The weight of the rule decides which rule will be used if multiple rules match")
   (inputs :initarg :inputs
           :reader inputs
           :initform (error "You need to specify at least one input")
           :documentation "A list of input-scanners that make this rule fire if they match")
   (actions :initarg :actions
            :reader actions
            :initform (list)
            :documentation "A procedure that is executed upon firing of the rule. You will have access to all the named matches")
   (fenders :initarg :fenders
            :reader fenders
            :initform (list)
            :documentation "A list of fenders that must be matched in order for this rule to be alowed to fire")
   (outputs :initarg :outputs
            :initform (list)
            :reader outputs
            :documentation "On or more output strings. Not that those are actually format expressions and you have access to the $ and % procedures")))

(defmethod print-object ((object <rule>) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name weight inputs actions fenders outputs) object
        (format stream "name: ~A weight: ~A input: ~A  output: ~A fenders: ~A" name weight inputs outputs fenders))))


@export
(defun rx-eval (input)
  "Run the input through our set of rules and fire if needed"
  (let ((rewritten-input (rewrite-query (denoise-query input))))
    (dbg "Rewrote query to: '~A'~%" rewritten-input)
    (multiple-value-bind (rule environment) (find-activation rewritten-input)
      (when rule
        (dbg "Activated rule: ~A~%" (name rule))
        (let ((*rule-environment* environment)
              (*current-rule* rule))
          (run-actions rule)
          (let ((answer (generate-answer rule)))
            (dbg "Generated answer ~A~%" answer)
            answer))))))

(defun rewrite-query (input)
  "Replace synonyms with their equivalents and thus rewrite the query"
  (format nil "~{~A~^ ~}" (mapcar #'synonym-for (tokenize-input input))))

(defun synonym-for (string &optional (default string))
  "Recursivly find the synonym for the given string"
  (let ((synonym (car (gethash string *synonyms*))))
    (if synonym
        (progn
          (dbg "Found synonym '~A' for '~A'~%" synonym string)
          (synonym-for synonym))
        default)))

(defun tokenize-input (input)
  "Split the input on whitespaces"
  (remove-if (lambda (str)
               (string-equal str ""))
             (split-sequence:split-sequence #\Space input)))

(defun punctuation-p (char)
  "Replace punctuations with neutral characters"
  (find char ".,;:`!?#-()\\\""))

(defun denoise-query (input)
  "Remove punctuations and downcase"
  (substitute-if  #\Space #'punctuation-p (string-downcase input)))

(defun find-activation (input)
  "Run through all the rules, eventually finding one that matches."
  
  (let ((matching-rules (list)))
    (dolist (rule *rules*)
      (dbg "Checking rule: <~A>~%" (name rule))
      (when (fenders-satisfied-p rule input)
        (multiple-value-bind (rule-matches bindings captures) (apply-scanners rule input)
          (when rule-matches
            (push (cons rule (make-environment bindings captures)) matching-rules)))))
    
    (when matching-rules
      (dbg "Have ~A matching rules. Selecting the one with greatest weight~%" (length matching-rules))
      (let ((activated-rule (car (sort matching-rules (lambda (lhs rhs) (> (weight (car lhs)) (weight (car rhs))))))))
        (values (car activated-rule) (cdr activated-rule))))))


(defun apply-scanners (rule input)
  (dolist (scanner (inputs rule))
    (dbg "Applying /~A/~%" (regexp-string scanner))
    (let ((rx (regexp scanner)))
      (multiple-value-bind (had-match bindings) (cl-ppcre:scan-to-strings rx input)
        (when had-match
          (dbg "Match! ~A~%" bindings)
          (return-from apply-scanners (values t bindings (captures scanner)))))))
  (values nil nil nil))

(defun fenders-satisfied-p (rule input)
  "Fenders are satisfied if either there are no fenders or all of them match the input"
  (let ((fenders (find-fenders (fenders rule))))
    (or (null fenders)
        (fenders-match-p fenders input))))

(defun find-fenders (fender-names)
  "Lookup the fenders by names and return the fender-instances"
  (loop for name in fender-names
     when (gethash name *fenders*)
     collect it))

(defun fenders-match-p (fenders input)
  "Checks that every scanner in the list of fenders matches the input"
  (every (lambda (fender)  (cl-ppcre:scan (regexp fender) input)) fenders))

(defun make-environment (bindings names)
  "Creates a new environment for rule-evaluation"
  (dbg "Creating environment Bindings:~A Names: ~A~%" bindings names)
  (let ((environment (make-hash-table)))
    (loop
       for key in names
       for match being the elements of bindings
       do
         (dbg "Binding ~A to ~A~%" key match)
         (setf (gethash key environment) match)
       finally
         (return environment))))

(defmacro funcall-if (expr)
  (let ((result (gensym)))
    `(let ((,result ,expr))
       (when ,result
         (funcall ,result)))))

(defun run-actions (rule)
  "Run the assoc"
  (dbg "Running actions with environment~%")
  (dump-hash-table *rule-environment*)
  (funcall-if (actions rule)))

(defun generate-answer (rule)
  (funcall-if (random-output rule)))

(defun random-output (rule)
  (with-slots (outputs) rule
    (nth (random (length outputs)) outputs)))

@export
(defmacro create-outputs (outputs)
  (let ((output-procs (mapcar (lambda (&rest arguments)
                                `(lambda ()
                                   (format nil ,@(car arguments))))
                              outputs)))
    `(list ,@output-procs)))

@export
(defmacro create-inputs (inputs)
  (let ((instantiations 
         (mapcar (lambda (input)
                   `(make-instance '<input-scanner>
                                   :regexp   ,(car input)
                                   :captures '(,@(cadr input))))
                 inputs)))
    `(list ,@instantiations)))

@export
(defmacro create-actions (actions)
  `(lambda ()  ,@actions))

@export
(defmacro defrule (name weight (&rest fender-names) &rest slots)
  (let ((inputs  (cdr (assoc :input  slots)))
        (actions (cdr (assoc :do slots)))
        (outputs (cdr (assoc :output slots))))

     `(push
       (make-instance '<rule>
                      :name (quote ,name)
                      :weight ,weight
                      :inputs  (create-inputs ,inputs)
                      :actions (create-actions ,actions)
                      :outputs (create-outputs ,outputs)
                      :fenders '(,@fender-names))
       *rules*)))

@export
(defmacro defsynonym ( base (synonym &rest more-synonyms))
  `(setf (gethash ,base *synonyms*) (list ,synonym ,@more-synonyms)))

@export
(defmacro deffender (name regexp)
  `(setf
    (gethash (quote ,name) *fenders*)
    (make-instance '<fender>
                   :name (quote ,name)
                   :regexp ,regexp)))

;; (reset-rules)
;; (deffender my-name "^my name is")

;; (defsynonym "vorname" ("name"))

;; (defrule my-name-is (my-name)
;;   (:input
;;    ("my name is (.*)" (name)))
;;   (:do
;;    (remember 'name ($ 'name)))
;;   (:output
;;    ("Hello ~A. Nice to meet you." ($ 'name))))
