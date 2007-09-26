(in-package #:kp6-cl)

(defclass kp6-Object ()
  ((bit :initarg :bit :accessor kp6-bit :initform t)))

(defgeneric kp6-true (object)
  (:documentation "Test OBJECT for truth.")
  (:method ((object kp6-Object))
    t))
