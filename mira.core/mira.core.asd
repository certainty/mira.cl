(in-package :cl-user)

(defpackage :mira.core.system
  (:use :cl :asdf))

(in-package :mira.core.system)

(defsystem :mira.core
    :version "0.1"
    :author "David Krentzlin"
    :depends-on (:log5 :cl-annot :bordeaux-threads :sb-concurrency :cl-ppcre :getopt :split-sequence)
    :description "Modular multithreaded bot framework"
    :license "BSD"
    :components ((:static-file "mira.core.asd")
                 (:module "src"
                          :components
                          ((:file "logging")
			   (:file "debug" :depends-on ("logging"))
                           (:file "utils" :depends-on ("debug"))
                           (:file "bootable" :depends-on ("debug"))
                           (:file "acl")
                           (:file "message-passing" :depends-on ("utils"))
                           (:file "message" :depends-on ("utils" "logging"))
                           (:file "channel" :depends-on ("message" "message-passing" "bootable" "logging"))
                           (:file "processor" :depends-on ("utils" "logging" "acl" "message"))
                           (:file "kernel" :depends-on ("channel" "message-passing" "processor" "bootable" "logging")))))
    :long-description
    #.(with-open-file (stream (merge-pathnames #p"README.md"
                                                (or *load-pathname* *compile-file-pathname*))
                               :if-does-not-exist nil
                               :direction :input)
         (when stream
           (let ((seq (make-array (file-length stream)
                                  :element-type 'character
                                  :fill-pointer t)))
             (setf (fill-pointer seq) (read-sequence seq stream))
             seq)))
    :in-order-to ((test-op
                   (load-op mira.core.test))))



