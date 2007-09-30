(in-package #:kp6-cl)

(defclass kp6-Code (kp6-Value)
  ((pad :initarg :pad :accessor kp6-pad)))

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
