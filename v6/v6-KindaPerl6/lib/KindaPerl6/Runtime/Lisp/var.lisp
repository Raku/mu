(in-package #:kp6-lisp)

(defun kp6-generate-variable (sigil name)
  (cons (find-symbol sigil 'kp6-lisp) name))

(dolist (sigil '($ @ % & |::|))
  (intern (string sigil) 'kp6-lisp))

(defgeneric kp6-default (sigil)
  (:documentation "Generates a default value for the given sigil."))

(defmethod kp6-default ((sigil (eql '$)))
  (make-instance 'kp6-Undef))

(defmethod kp6-default ((sigil (eql '@)))
  (make-instance 'kp6-Array))

(defmethod kp6-default ((sigil (eql '%)))
  (make-instance 'kp6-Hash))

(defmethod kp6-default ((sigil (eql '&)))
  (make-instance 'kp6-Undef))
