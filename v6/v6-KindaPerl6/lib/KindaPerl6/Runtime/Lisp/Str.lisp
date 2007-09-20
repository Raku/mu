(in-package #:kp6-cl)

; TODO: subclass Value
(defclass Str ()
  ((value :initarg :value)))
  
(defmethod str ((s Str)) s)
