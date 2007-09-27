(in-package #:kp6-cl)

(defclass kp6-Container (kp6-Object)
  ((value :initarg :value :accessor kp6-value)))

(defgeneric kp6-lookup (object index &key &allow-other-keys)
  (:documentation "Look up INDEX in OBJECT."))

(defgeneric kp6-store (object index value &key &allow-other-keys)
  (:documentation "Store VALUE in OBJECT at INDEX."))

(defgeneric (setf kp6-lookup) (new-value object index &key &allow-other-keys)
  (:documentation "Store VALUE in OBJECT at INDEX.")
  (:method (value (object kp6-Container) index &key)
    (kp6-store object index value)))
