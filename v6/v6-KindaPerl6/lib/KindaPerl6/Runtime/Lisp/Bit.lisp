(in-package #:kp6-cl)

; XXX: Make this support `t' and `nil' and 1 and 0
(defclass kp6-Bit (kp6-Value)
  ((bit :initarg :value)))
 
(defmethod initialize-instance :after ((self kp6-Bit) &key)
  (when (= (kp6-value self) 0) (setf (kp6-bit self) nil)))

