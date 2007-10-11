(in-package #:kp6-lisp-user)

(defvar *kp6-programs* nil "List of programs to execute.")

(defmacro kp6-add-program ((interpreter) &body body)
  `(push
    #'(lambda (,interpreter)
        (with-kp6-interpreter (,interpreter)
          ,@body))
    *kp6-programs*))
  
(defun main (&key (interpreter (make-instance 'kp6-interpreter)) (standalone t))
  (kp6-check-implementation-compatibility)
  (kp6-initialize-interpreter interpreter)
  (handler-case (dolist (program (reverse *kp6-programs*))
                  (funcall program interpreter))
    (error (e)
      (when standalone (kp6-quit interpreter :message (format nil "Error: ~A~%" e) :code 1))))
  (when standalone (kp6-quit interpreter)))
