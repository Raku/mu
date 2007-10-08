(in-package #:kp6-lisp)

(defclass kp6-Array (kp6-Container)
  ((value :initform (make-array 1 :adjustable t))))

(defmethod kp6-lookup ((self kp6-Array) index &key)
  (assert (typep index 'integer) (index))
  (elt (kp6-value self) index))

(defmethod kp6-store ((self kp6-Array) index value &key)
  (assert (typep index 'integer) (index))
  (when (<= index (array-dimension (kp6-value self) 0))
    (adjust-array (kp6-value self) (1+ index)))
  (setf (elt (kp6-value self) index) value))
