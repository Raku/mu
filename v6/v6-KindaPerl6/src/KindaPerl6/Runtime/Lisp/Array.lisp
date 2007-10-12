(in-package #:kp6-lisp)

(defclass kp6-Array (kp6-Container)
  ((value :initform (make-array 0 :adjustable t))))


(defmethod kp6-lookup ((self kp6-Array) index &key)
  ;; this may be phased out?
  (assert (typep index 'integer) (index))
  (elt (kp6-value self) index))

(defmethod kp6-store ((self kp6-Array) index value &key)
  ;; this may be phased out?
  (assert (typep index 'integer) (index))
  (when (<= index (array-dimension (kp6-value self) 0))
    (adjust-array (kp6-value self) (1+ index)))
  (setf (elt (kp6-value self) index) value))


(defmethod kp6-dispatch ((invocant kp6-Array) interpreter (method (eql :lookup)) &rest parameters)
  "Retrieve a value from the array"
  (declare (ignore interpreter))
  (assert (= 1 (length parameters)))
  (let ((index (perl->cl (elt parameters 0))))
    (elt (kp6-value invocant) index)))

(defmethod kp6-dispatch ((invocant kp6-Array) interpreter (method (eql :store)) &rest parameters)
  "Stores a value in the index of the array"
  (declare (ignore interpreter))
  (assert (= 2 (length parameters)))
  (let ((index (perl->cl (elt parameters 0)))
	(value (elt parameters 1)))
    (when (<= index (array-dimension (kp6-value invocant) 0))
      (adjust-array (kp6-value invocant) (1+ index)))
    (setf (elt (kp6-value invocant) index) value)))

(defmethod kp6-dispatch ((invocant kp6-Array) interpreter (method (eql :push)) &rest parameters)
  "Stores a value in the index of the array"
  (declare (ignore interpreter))
  (assert (= 1 (length parameters)))
  (let ((index (array-dimension (kp6-value invocant) 0))
	(value (elt parameters 0)))
    (adjust-array (kp6-value invocant) (1+ index))
    (setf (elt (kp6-value invocant) index) value)))

(defmethod kp6-dispatch ((invocant kp6-Array) interpreter (method (eql :elems)) &rest parameters)
  "Returns the number of elements in the array"
  (declare (ignore parameters interpreter))
  (make-instance 'kp6-Int
     :value (array-dimension (kp6-value invocant) 0)))
