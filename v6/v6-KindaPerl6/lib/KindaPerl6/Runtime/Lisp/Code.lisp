(in-package #:kp6-cl)

(defclass kp6-code ()
  ((value :initarg :value)))

(defun my-APPLY (self arg)
  (funcall (slot-value self 'value) (make-instance 'kp6-str :value arg)))
