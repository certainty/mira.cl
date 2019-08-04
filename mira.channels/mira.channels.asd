(in-package :cl-user)

(defpackage :mira.channels.system
  (:nicknames :mic.system)
  (:use :cl :asdf))

(in-package :mira.channels.system)

(defsystem :mira.channels
    :version "0.1"
    :author "David Krentzlin"
    :depends-on (:mira.core :cl-irc :cl+ssl :anaphora :usocket trivial-utf-8)
    :description "Some common channels for mira"
    :components ((:static-file "mira.channels.asd")
                 (:module :src
                          :components
                          ((:file "stdio")
                           (:file "irc")
                           (:file "tcp")))))
