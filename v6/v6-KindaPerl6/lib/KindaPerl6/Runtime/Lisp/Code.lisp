(in-package #:kp6-cl)

(defclass kp6-Code (kp6-Value)
  ())

(defun is-kp6-code (object)
  (typep object 'kp6-Code))

(defun kp6-apply (self arg)
  (funcall (kp6-value self) arg))
