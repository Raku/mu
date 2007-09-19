(defpackage :GLOBAL)
(in-package :GLOBAL)

(defparameter say (make-instance 'Code :value #'(

    (lambda (&rest strs)
      (loop for str in strs collect
           (format t "~a" (slot-value  'value)))
      (format t "~%")
      
      ; return Bool::True
      (make-instance 'Bit :value 1)
    )

)))

