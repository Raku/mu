(in-package #:kp6-cl)

(defclass kp6-Package (kp6-Hash)
  ())

(defun is-kp6-package (object)
  (typep object 'kp6-Package))
