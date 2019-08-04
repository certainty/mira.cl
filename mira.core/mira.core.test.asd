(in-package :cl-user)
(defpackage :mira.core.test.system
  (:use :cl :asdf))
(in-package :mira.core.test.system)

(defsystem :mira.core.test
  :author "David Krentzlin"
  :depends-on (:FiveAM :cl-annot :mira.core)
  :description "Tests for mira"
  :license "BSD"
  :components ((:static-file "mira.core.test.asd")
               (:module "t"
                        :components
                        ((:file "utils-test")))))
