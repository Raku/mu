(in-package #:kp6-cl)

(defclass kp6-signature (kp6-Object)
  ((invocant :accessor kp6-invocant :initarg :invocant)
   (return :accessor kp6-return :initarg :return)
   (array :accessor kp6-array :initarg :array)
   (hash :accessor kp6-hash :initarg :hash)))
