(in-package #:kp6-lisp)

(defclass kp6-Code (kp6-Value)
  ((signature :accessor kp6-signature :initarg :signature)))

(defmacro make-kp6-sub ((signature &key (interpreter (gensym))) &body body)
  (with-unique-names (rest signature-var)
    `(let ((,signature-var ,signature))
      (make-instance
       'kp6-code
       :value #'(lambda (,interpreter &rest ,rest)
		  (declare (ignorable ,interpreter))
		  (with-kp6-arguments (,interpreter ,signature-var ,rest)
		    ,@body))
       :signature ,signature))))

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
    (let ((args (ensure-list args)))
      (if (typep obj 'function)
	  (apply obj interpreter args)
	  (let* ((function (kp6-find-function interpreter obj package))
		 (function-object (when (typep function 'kp6-cell) (kp6-cell-value function))))
	    (unless (is-kp6-code function-object)
	      (kp6-error interpreter 'kp6-function-not-found :package package :function obj))
	    (apply (kp6-value function-object) interpreter args))))))

(defmethod kp6-dispatch ((invocant kp6-Code) interpreter (method (eql :apply)) &rest parameters)
  ".() on a function object"
  (let ((parameters (mapcar #'make-kp6-positional-parameter parameters)))
    (apply (kp6-value invocant) interpreter parameters)))

;; AUTHORS
;;
;; The Pugs Team perl6-compiler@perl.org.
;;
;; SEE ALSO
;;
;; The Perl 6 homepage at http://dev.perl.org/perl6.
;;
;; The Pugs homepage at http://pugscode.org/.
;;
;; COPYRIGHT
;;
;; Copyright 2007 by Flavio Soibelmann Glock and others.
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the same terms as Perl itself.
;;
;; See http://www.perl.com/perl/misc/Artistic.html
