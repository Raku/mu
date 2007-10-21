(in-package #:kp6-lisp)

(defclass kp6-Undef (kp6-Value)
  ((value :initform nil)))

(defmethod kp6-dispatch ((invocant kp6-Undef) interpreter (method (eql :true)) &rest parameters)
  (declare (ignore parameters interpreter))
  (make-instance 'kp6-Bit :value nil))
