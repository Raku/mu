(in-package #:kp6-cl)

(defclass kp6-Undef (kp6-Value)
  ((value :initform nil)))

(defmethod kp6-true ((object kp6-Undef))
  nil)
