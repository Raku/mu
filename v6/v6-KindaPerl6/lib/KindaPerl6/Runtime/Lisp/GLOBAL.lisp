(defpackage :GLOBAL)
(in-package :GLOBAL)

(defun say (&rest strs)
  (loop for str in strs collect
       (format t "~a" (slot-value  'value)))
  (format t "~%")
  ; XXX: Should this return T or Bool::True?
  t)
