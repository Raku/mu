(in-package #:kp6-lisp)

(defclass kp6-Bit (kp6-Value)
  ())

(defmethod kp6-dispatch ((invocant kp6-Bit) (method (eql :true)) &rest parameters)
  (declare (ignore parameters))
  (make-instance 'kp6-Bit :value (not (null (kp6-dispatch invocant :cl-landish)))))

(defmethod kp6-dispatch ((invocant kp6-Bit) (method (eql :cl-landish)) &rest parameters)
  "Return a lisp object for the Bit, i.e. `nil' or `t'"
  (declare (ignore parameters))
  (not (null (slot-value invocant 'value))))

