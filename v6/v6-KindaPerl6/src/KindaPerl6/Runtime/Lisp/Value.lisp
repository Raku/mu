(in-package #:kp6-lisp)

(defclass kp6-Value (kp6-Object)
  ((value
    :initarg :value
    :accessor kp6-value)))

(defmethod kp6-dispatch ((invocant kp6-Value) interpreter (method (eql :cl-landish)) &rest parameters)
  "Return a lisp object for the value, this is the slot value unless
the inheritors want to do some further munging."
  (declare (ignore parameters interpreter))
  (slot-value invocant 'value))
