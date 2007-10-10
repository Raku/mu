(in-package #:kp6-lisp)

(defclass kp6-Num (kp6-Value)
  ())

(defmethod kp6-true ((object kp6-Num))
  (/= (kp6-value object) 0))

(defmethod kp6-dispatch ((invocant kp6-Num) (method (eql :true)) &rest parameters)
  (declare (ignore parameters))
  (make-instance 'kp6-Bit :value (/= (kp6-value invocant) 0)))
