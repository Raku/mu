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

(defmacro kp6-for-->-single (loop-variable array &body body)
  (with-unique-names (array-index)
    `(dotimes (,array-index (array-dimension (kp6-value ,array) 0))
       (set-lexical-variable ,loop-variable (kp6-lookup ,array ,array-index))
       ,@body)))
