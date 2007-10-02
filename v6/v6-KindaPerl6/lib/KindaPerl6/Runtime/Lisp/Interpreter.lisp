(in-package #:kp6-cl)

(defclass kp6-interpreter ()
  ((packages :accessor kp6-packages :initarg :packages :initform (make-instance 'kp6-Hash))))

(defmethod initialize-instance :after ((interpreter kp6-interpreter) &rest initargs)
  (declare (ignore initargs))
  (kp6-create-package interpreter "GLOBAL"))

(defmacro with-kp6-interpreter ((name &rest args) &body body)
  `(let ((,name (make-instance 'kp6-interpreter ,@args)))
    (kp6-initialize-interpreter ,name)
    ,@body))
