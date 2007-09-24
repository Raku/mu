(in-package #:kp6-cl)

(defclass kp6-Pad (kp6-Hash)
  ((parent :accessor kp6-parent)))

(defun kp6-generate-variable (sigil name)
  (cons (find-symbol sigil 'kp6-cl) name))

(dolist (sigil '($ @ % & |::|))
  (intern (string sigil) 'kp6-cl))
