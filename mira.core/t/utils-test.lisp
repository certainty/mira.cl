(in-package :cl-user)
(defpackage :mira.core.test.utils-test
  (:use :cl :FiveAM))

(in-package :mira.core.test.utils-test)

(defcommand processor :access-level 0 (arg1 arg2 &key (foo)))
