(in-package #:kp6-cl)

(defclass kp6-Num (kp6-Value)
  ())

(defmethod initialize-instance :after ((obj kp6-Num) &key)
  (when (= (kp6-value obj) 0) (setf (kp6-bit obj) nil)))

(defmethod (setf kp6-value) :after (val (obj kp6-Num))
  (when (= val 0) (setf (kp6-bit obj) nil)))
