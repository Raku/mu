(in-package #:kp6-lisp)

(defclass kp6-Int (kp6-Num)
  ())

(defmethod kp6-dispatch ((invocant kp6-Int) interpreter (method (eql :Str)) &rest parameters)
  "Stringify the Int with `format'"
  (declare (ignore parameters interpreter))
  (make-instance 'kp6-Str :value (format nil "~d" (slot-value invocant 'value))))

(defmethod kp6-dispatch ((invocant kp6-Int) interpreter (method (eql :Int)) &rest parameters)
  "Intify the Int"
  (declare (ignore parameters interpreter))
  invocant)
