(defpackage :GLOBAL)
(in-package :GLOBAL)

;; FIXME: Make this variadic and have it accept a filehandle other
;; than stdout
(defun say (str)
  (format t "~a~%" str)
  t)
