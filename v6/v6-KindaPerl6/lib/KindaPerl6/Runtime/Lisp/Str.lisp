(in-package #:kp6-cl)

(defclass kp6-str (kp6-value)
  ())
  
(defmethod str ((s kp6-str)) s)
