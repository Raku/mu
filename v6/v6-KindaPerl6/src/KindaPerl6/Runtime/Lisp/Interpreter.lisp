(in-package #:kp6-lisp)

(defclass kp6-interpreter ()
  ((packages :accessor kp6-packages :initarg :packages :initform (make-instance 'kp6-Hash))))

(defmethod initialize-instance :after ((interpreter kp6-interpreter) &rest initargs)
  (declare (ignore initargs))
  (kp6-create-package interpreter "GLOBAL"))

(defmacro with-kp6-interpreter ((interpreter) &body body)
  (let ((functions (mapcar
                    #'(lambda (func) `(,func (&rest rest)
                                       (declare (ignore rest))
                                       (kp6-error ,interpreter 'kp6-stub-function :name ',func)))
                    '(enclosing-pad outer-pad lexical-variable-exists define-lexical-variable set-lexical-variable set-lexical-variable/c lookup-lexical-variable lookup-lexical-variable/c define-our-variable))))
    `(flet ,functions
      (declare (ignorable ,@(mapcar #'(lambda (x) `#',(first x)) functions)))
      ,@body)))

(defmacro kp6-for-loop-structure ((interpreter loop-variable array) &body body)
  (with-unique-names (array-index array-value)
    `(let ((,array-value ,array))
       (dotimes (,array-index (perl->cl (kp6-dispatch ,array-value ,interpreter :elems)))
	 (set-lexical-variable ,loop-variable
			       (kp6-dispatch ,interpreter ,array-value :lookup
					     (make-instance 'kp6-Int :value ,array-index)))
	 ,@body))))
