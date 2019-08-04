(in-package :cl-user)

(defpackage :mira.processors.system
  (:nicknames :mip.system)
  (:use :cl :asdf))

(in-package :mira.processors.system)

(defsystem :mira.processors
    :version "0.1"
    :author "David Krentzlin"
    :depends-on (:mira.core :cl-ppcre :split-sequence :km)
    :description "Some common processors for mira"
    :components ((:static-file "mira.processors.asd")
                 (:module :src
                          :components
                          ((:module :support
                                    :components
                                    ((:file "regexp-machine")))
                           (:file "echo")
                           (:file "pipe")
                           (:file "chat" :depends-on ("support"))
                           (:file "km")))))
