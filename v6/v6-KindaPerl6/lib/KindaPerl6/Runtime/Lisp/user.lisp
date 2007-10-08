(in-package #:kp6-lisp-user)

(defvar *kp6-programs* nil "List of programs to execute.")

(defmacro kp6-add-program ((interpreter) program)
  `(push
    #'(lambda (,interpreter)
        (with-kp6-interpreter (,interpreter)
          ,program))
    *kp6-programs*))
  
(defun main ()
  (kp6-check-implementation-compatibility)
  (let ((interpreter (make-instance 'kp6-interpreter)))
    (kp6-initialize-interpreter interpreter)
    (handler-case (dolist (program (reverse *kp6-programs*))
                    (funcall program interpreter))
      (error (e)
        (kp6-quit interpreter :message (format nil "Error: ~A~%" e) :code 1)))
    (kp6-quit interpreter)))
