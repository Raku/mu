(in-package #:kp6-cl)

(defclass kp6-Thunk ()
  ((code :reader kp6-code :initarg :code)))

(defmacro kp6-delay (&body body)
  `(make-instance 'kp6-Thunk :code #'(lambda () ,@body)))

(defun kp6-force (thunk)
  (assert (typep thunk 'kp6-Thunk) (thunk) "First argument to KP6-FORCE must be a KP6-THUNK, not a ~A." (type-of thunk))
  (funcall (kp6-code thunk)))
