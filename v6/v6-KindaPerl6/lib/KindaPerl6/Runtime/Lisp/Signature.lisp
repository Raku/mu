(in-package #:kp6-cl)

(defclass kp6-signature (kp6-Object)
  ((invocant :accessor kp6-invocant :initarg :invocant)
   (return :accessor kp6-return :initarg :return)
   (value :accessor kp6-value :initarg :value)))

(defun kp6-sig-item (type name)
  (cons type name))
