(defpackage "Code")

(defclass Code ()
  ((value :initarg :value)))

(defun my-APPLY (self arg)
  (funcall (slot-value self 'value) (make-instance 'Str :value arg)))
