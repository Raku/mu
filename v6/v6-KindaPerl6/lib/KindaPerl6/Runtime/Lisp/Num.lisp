(in-package #:kp6-cl)

(defclass kp6-Num (kp6-Value)
  ())

(defmethod kp6-true ((object kp6-Num))
  (/= (kp6-value object) 0))
