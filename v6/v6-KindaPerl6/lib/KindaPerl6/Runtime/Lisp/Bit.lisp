(in-package #:kp6-cl)

(defclass kp6-Bit (kp6-Value)
  ())

(defmethod kp6-true ((object kp6-Bit))
  (not (null (kp6-value object))))
