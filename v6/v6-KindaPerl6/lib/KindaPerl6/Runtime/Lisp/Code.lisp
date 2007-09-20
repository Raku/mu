(in-package #:kp6-cl)

(defclass kp6-code ()
  ((value :initarg :value :accessor kp6-value)))

(defun kp6-apply (self arg)
  (funcall (kp6-value self) arg))
