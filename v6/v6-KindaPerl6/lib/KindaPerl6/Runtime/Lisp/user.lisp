(in-package #:kp6-lisp-user)

(defvar *kp6-programs* nil "List of programs to execute.")

(defmacro kp6-add-program (program)
  `(push #'(lambda () ,program) *kp6-programs*))
  
(defun main ()
  (dolist (program (reverse *kp6-programs*))
    (funcall program)))
