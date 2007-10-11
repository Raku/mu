(in-package #:kp6-lisp)

(defun kp6-check-implementation-compatibility ()
  t)

(defmethod kp6-quit ((interpreter kp6-interpreter) &key message (code 0))
  (when message (write-string message))
  (sb-ext:quit :unix-status code))
