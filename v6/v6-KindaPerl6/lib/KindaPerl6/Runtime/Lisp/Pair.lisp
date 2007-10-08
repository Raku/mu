(in-package #:kp6-lisp)

(defclass kp6-pair (kp6-Object)
  ((key :accessor kp6-key :initarg :key)
   (value :accessor kp6-value :initarg :value)))
