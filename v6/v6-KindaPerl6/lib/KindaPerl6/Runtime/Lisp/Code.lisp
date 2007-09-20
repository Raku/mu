(in-package #:kp6-cl)

(defclass kp6-code ()
  ((value :initarg :value)))

(defun kp6-APPLY (self arg)
  (funcall (slot-value self 'value) arg))
