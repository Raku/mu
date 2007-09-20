(in-package #:kp6-cl)

(defparameter say (make-instance 'Code
  :value #'(lambda (&rest strs)
             (loop for str in strs collect
                  (format t "~a" (slot-value str 'value)))
             (format t "~%")
             ; return Bool::True
             (make-instance :Bit :value 1))))
