(cl:in-package cl)
(defpackage Main)

(defun Main::substr (s start count) 
  (subseq s start (+ start count)))

(if (not (ignore-errors (find-method 'Main::bool () ())))
  (defgeneric Main::bool (self)
      (:documentation "get a bool value")))
(defmethod Main::bool (x) x)
(defmethod Main::bool ((x number)) (not (eql x 0)))
(defmethod Main::bool ((x string)) (and (not (equal x "")) (not (equal x "0"))))

