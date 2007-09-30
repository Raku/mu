(in-package #:kp6-cl)

(defclass kp6-Container (kp6-Object)
  ((value :initarg :value :accessor kp6-value)))

(define-condition kp6-bad-index (kp6-error)
  ((got :reader kp6-got :initarg :got)
   (expected :reader kp6-expected :initarg :expected))
  (:report (lambda (c s)
	     (write-string (kp6-prefixed-error-message c "Expected ~A index, got: ~A" (kp6-expected c) (kp6-got c)) s))))

(defgeneric kp6-lookup (object index &key &allow-other-keys)
  (:documentation "Look up INDEX in OBJECT."))

(defgeneric kp6-store (object index value &key &allow-other-keys)
  (:documentation "Store VALUE in OBJECT at INDEX."))

(defgeneric (setf kp6-lookup) (new-value object index &key &allow-other-keys)
  (:documentation "Store VALUE in OBJECT at INDEX.")
  (:method (value (object kp6-Container) index &key)
    (kp6-store object index value)))

(defgeneric kp6-exists (object index &key &allow-other-keys)
  (:documentation "Check whether INDEX exists in OBJECT."))

(defgeneric kp6-delete (object index &key &allow-other-keys)
  (:documentation "Delete value of INDEX from OBJECT."))

(defgeneric kp6-clear (object &key &allow-other-keys)
  (:documentation "Empty OBJECT."))

(defgeneric kp6-pairs (object &key &allow-other-keys)
  (:documentation "List KEY/VALUE pairs in OBJECT."))

(defgeneric kp6-elems (object &key &allow-other-keys)
  (:documentation "Count elements in OBJECT."))
