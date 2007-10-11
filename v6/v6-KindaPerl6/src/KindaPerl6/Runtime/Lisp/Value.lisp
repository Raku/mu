(in-package #:kp6-lisp)

(defclass kp6-Value (kp6-Object)
  ((value
    :initarg :value
    :accessor kp6-value)))
