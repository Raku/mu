(in-package #:kp6-cl)

(defclass kp6-Code (kp6-Value)
  ())

(defun kp6-apply (self arg)
  (funcall (kp6-value self) arg))
