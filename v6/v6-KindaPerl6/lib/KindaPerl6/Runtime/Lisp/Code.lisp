(in-package #:kp6-cl)

(defclass kp6-Code (kp6-Value)
  ())

(defun is-kp6-code (object)
  (typep object 'kp6-Code))
