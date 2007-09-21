; lookup the &GLOBAL::say variable,
; and then store the Code object into it
(in-package #:kp6-cl)

(store
 (kp6-lookup (kp6-lookup 'kp6-Hash_KP6 (make-instance 'kp6-str :value "GLOBAL"))
             (make-instance 'kp6-str :value "Code_say"))

(make-instance
 'kp6-code
 :value #'(lambda (&rest strs)
	    (dolist (str strs)
	      (format t "~A" (kp6-value str)))
	    (format t "~%")
	    (make-instance 'kp6-bit :value 1))))
