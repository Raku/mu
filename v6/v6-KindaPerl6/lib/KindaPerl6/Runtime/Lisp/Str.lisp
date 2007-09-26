(in-package #:kp6-cl)

(defclass kp6-Str (kp6-Value)
  ())

(defmethod kp6-true ((object kp6-Str))
  (string/= (kp6-value object) ""))
