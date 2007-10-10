(in-package #:kp6-lisp)

(defclass kp6-Bit (kp6-Value)
  ())

(defmethod kp6-true ((object kp6-Bit))
  (not (null (kp6-value object))))

(defmethod kp6-dispatch ((invocant kp6-Bit) (method (eql :true)) &rest parameters)
  (declare (ignore parameters))
  (make-instance 'kp6-Bit :value (not (null (kp6-value invocant)))))

(defmethod kp6-dispatch ((invocant kp6-Bit) (method (eql :cl-landish)) &rest parameters)
  (declare (ignore parameters))
  (not (null (kp6-value invocant))))