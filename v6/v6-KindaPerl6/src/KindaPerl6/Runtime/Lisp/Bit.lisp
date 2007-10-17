(in-package #:kp6-lisp)

(defclass kp6-Bit (kp6-Value)
  ())

(defmethod kp6-dispatch ((invocant kp6-Bit) interpreter (method (eql :true)) &rest parameters)
  (declare (ignore parameters))
  (make-instance 'kp6-Bit :value (not (null (kp6-dispatch invocant interpreter :cl-landish)))))

(defmethod kp6-dispatch ((invocant kp6-Bit) interpreter (method (eql :cl-landish)) &rest parameters)
  "Return a lisp object for the Bit, i.e. `nil' or `t'"
  (declare (ignore parameters interpreter))
  (not (null (slot-value invocant 'value))))

(defmethod kp6-dispatch ((invocant kp6-Bit) interpreter (method (eql :Num)) &rest parameters)
  "Coerce the Bit to a number, i.e. `0' or `1'"
  (declare (ignore parameters ))
  (make-instance 'kp6-Num :value (if (kp6-dispatch invocant interpreter :cl-landish)
                                     1
                                     0)))

(defmethod kp6-dispatch ((invocant kp6-Bit) interpreter (method (eql :Str)) &rest parameters)
  "Coerce the Bit to a number, i.e. `0' or `1'"
  (declare (ignore parameters ))
  (make-instance 'kp6-Num :value (if (kp6-dispatch invocant interpreter :cl-landish)
                                     "Bool::True"
                                     "Bool::False")))
