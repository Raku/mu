(defpackage #:kp6-GLOBAL
  (:use #:cl #:kp6-cl))
(in-package #:kp6-GLOBAL)

(defparameter kp6-Code_say (make-instance 'kp6-code
  :value #'(lambda (&rest strs)
	     (dolist (str strs)
	       (format t "~A" (slot-value str 'value)))
             (format t "~%")
             ; return Bool::True
             (make-instance 'kp6-bit :value 1))))
