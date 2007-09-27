(in-package #:kp6-cl)

(defclass kp6-Array (kp6-Container)
  ((value :initform (make-array 1 :adjustable t :fill-pointer t))))

(defmethod kp6-lookup ((self kp6-Array) index &key)
  (assert (typep index 'integer) (index))
  (elt (kp6-value self) index))

(defmethod kp6-store ((self kp6-Array) index value &key)
  (assert (typep index 'integer) (index))
  (setf (elt (kp6-value self) index) value))
