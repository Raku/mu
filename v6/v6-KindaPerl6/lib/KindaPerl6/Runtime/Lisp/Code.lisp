(in-package #:kp6-cl)

(defclass kp6-Code (kp6-Value)
  ((signature :accessor kp6-signature :initarg :signature)))

(define-condition kp6-bad-arguments (kp6-error)
  ((signature :accessor kp6-signature :initarg :signature)
   (arguments :accessor kp6-arguments :initarg :arguments))
  (:report (lambda (c s)
	     (write-string (kp6-prefixed-error-message c "Bad arguments.  Expected: ~A~%Got: ~A" (kp6-signature c) (kp6-arguments c)) s))))

(defmacro make-kp6-sub ((signature &key (interpreter (gensym))) &body body)
  (with-unique-names (rest)
    `(make-instance
      'kp6-code
      :value #'(lambda (,interpreter &rest ,rest)
		 (with-kp6-arguments (,interpreter ,signature ,rest)
		   ,@body))
      :signature ,signature)))

(defmacro with-kp6-arguments ((interpreter signature arguments) &body body)
  (with-unique-names (value item argument)
    `(with-kp6-pad (,interpreter)
      (let ((,value (kp6-value ,signature)))
	(unless (= (length ,value) (length ,arguments))
	  (kp6-error ,interpreter 'kp6-bad-arguments :signature ,value :arguments ,arguments))
	(loop
	 :for ,item :in ,value
	 :for ,argument :in ,arguments
	 :do (kp6-check-parameter ,interpreter ,item ,argument)
	 :do (define-lexical-variable (cdr ,item))
	 :do (set-lexical-variable (cdr ,item) ,argument))
	,@body))))

(defgeneric kp6-check-parameter (interpreter parameter argument &key &allow-other-keys)
  (:method ((interpreter kp6-interpreter) parameter argument &key)
    t))

(defun is-kp6-code (object)
  (typep object 'kp6-Code))

(define-condition kp6-function-not-found (kp6-error)
  ((function :reader kp6-function :initarg :function)
   (package :reader kp6-package :initarg :package))
  (:report (lambda (c s)
	     (write-string (kp6-prefixed-error-message c "Function ~A not found in package ~A." (kp6-function c) (kp6-package c)) s))))

(defun kp6-normalize-function-name (name)
  (cons '& name))

(defgeneric kp6-find-function (interpreter name &optional package)
  (:method ((interpreter kp6-interpreter) name &optional (package "GLOBAL"))
    (let ((package-object (kp6-find-package interpreter package)))
      (unless (is-kp6-package package-object)
	(kp6-error interpreter 'kp6-package-not-found :package package))
      (kp6-lookup package-object name))))

(defgeneric kp6-apply-function (interpreter obj args &optional package)
  (:method ((interpreter kp6-interpreter) obj args &optional (package "GLOBAL"))
      (if (typep obj 'function)
	  (apply obj interpreter args)
	  (let* ((function (kp6-find-function interpreter obj package))
		 (function-object (when (typep function 'kp6-cell) (kp6-cell-value function))))
	    (unless (is-kp6-code function-object)
	      (kp6-error interpreter 'kp6-function-not-found :package package :function obj))
	    (apply (kp6-value function-object) interpreter args)))))
