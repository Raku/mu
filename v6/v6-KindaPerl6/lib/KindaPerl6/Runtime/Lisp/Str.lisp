(in-package #:kp6-cl)

(defclass kp6-Str (kp6-Value)
  ())

(defmethod str ((self kp6-Str))
  "Stringify the string object, just fetch the boxed value (is this even needed?)"
  (kp6-value self))
