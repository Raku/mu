(in-package #:kp6-cl)

(defclass kp6-Str (kp6-Value)
  ())

(defgeneric str (self)
  (:documentation "Stringify the string object, just fetch the boxed value (is this even needed?)")
  (:method ((self kp6-Str))
    self))
