(in-package #:cl-user)
(load (compile-file "lib/KindaPerl6/Runtime/Lisp/Runtime.lisp"))
(defpackage #:Main
  (:use #:cl #:kp6-cl))
(in-package #:Main)
;; Do not edit this file - Lisp generated by KindaPerl6
(defconstant +KP6_DISABLE_INSECURE_CODE+ nil)
( kp6-APPLY kp6-GLOBAL::kp6-Code_say (make-instance 'kp6-str :value "hello") )
