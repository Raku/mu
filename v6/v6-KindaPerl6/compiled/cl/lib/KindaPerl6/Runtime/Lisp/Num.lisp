(in-package #:kp6-lisp)

(defclass kp6-Num (kp6-Value)
  ())

(defmethod kp6-dispatch ((invocant kp6-Num) interpreter (method (eql :true)) &rest parameters)
  (declare (ignore parameters interpreter))
  (make-instance 'kp6-Bit :value (/= (kp6-value invocant) 0)))

(defmethod kp6-dispatch ((invocant kp6-Num) interpreter (method (eql :Num)) &rest parameters)
  (declare (ignore parameters interpreter))
  invocant)
