(in-package #:kp6-cl)

(defclass kp6-Value (kp6-Object)
  ((value
    :initarg :value
    :accessor kp6-value)))

; Overriden in subclasses
(defmethod kp6-Str ((self kp6-Value)) self)
(defmethod kp6-Num ((self kp6-Value)) self)
