(in-package #:kp6-cl)

(defclass kp6-Object ()
  ())

(defgeneric kp6-true (object)
  (:documentation "Test OBJECT for truth.")
  (:method ((object kp6-Object))
    t))
