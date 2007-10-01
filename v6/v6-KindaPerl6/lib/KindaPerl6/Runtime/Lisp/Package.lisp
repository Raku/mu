
(in-package #:kp6-cl)

(defconstant +kp6-default-package+ "GLOBAL")

(defclass kp6-Package (kp6-Pad)
  ())

(defun is-kp6-package (object)
  (typep object 'kp6-Package))

(define-condition kp6-package-condition (kp6-condition)
  ((package :reader kp6-package :initarg :package)))

(define-condition kp6-package-exists (kp6-warning kp6-package-condition)
  ()
  (:report (lambda (c s)
	     (write-string (kp6-prefixed-error-message c "Package ~A already exists." (kp6-package c)) s))))

(define-condition kp6-package-not-found (kp6-error kp6-package-condition)
  ()
  (:report (lambda (c s)
	     (write-string (kp6-prefixed-error-message c "Package ~A does not exist." (kp6-package c)) s))))

(define-condition kp6-package-variable-not-found (kp6-warning)
  ((name :accessor kp6-name :initarg :name)
   (package :accessor kp6-package :initarg :package))
  (:report (lambda (c s)
	     (write-string (kp6-prefixed-error-message c "Variable ~A not found in package ~A." (kp6-name c) (kp6-package c)) s))))

(defgeneric kp6-create-package (interpreter name)
  (:documentation "Create a new Perl 6 package.")
  (:method ((interpreter kp6-interpreter) name)
    (with-accessors ((packages kp6-packages)) interpreter
		    (when (kp6-lookup packages name)
		      (kp6-warn interpreter 'kp6-package-exists :package name))
		    (kp6-store packages name (make-instance 'kp6-Package)))))

(defgeneric kp6-find-package (interpreter name)
  (:documentation "Find a Perl 6 package.  Returns NIL if not found.")
  (:method ((interpreter kp6-interpreter) name)
    (kp6-lookup (kp6-packages interpreter) name)))

(defmacro with-kp6-package ((interpreter package pad &optional parent-pad) &body body)
  (with-unique-names (interpreter-var package-var)
    (let ((functions (kp6-with-package-functions package-var interpreter-var package)))
      `(let* ((,interpreter-var ,interpreter)
	      (,package-var (kp6-lookup (kp6-packages ,interpreter-var) ,package)))
	(flet ,functions
	  (declare (ignorable ,@(mapcar #'(lambda (func) `#',(car func)) functions)))
	  (with-kp6-pad (,interpreter-var ,pad ,@(when parent-pad `(:parent ,parent-pad)))
	    ,@body))))))

(defun kp6-with-package-functions (package-var interpreter-var package-name)
  `((enclosing-package () ,package-name)
    (define-package-variable (name &optional (package ,package-var) type)
	(declare (ignore type))
      (let ((package-object (kp6-find-package ,interpreter-var package)))
	(when (kp6-exists package-object name)
	  (kp6-error ,interpreter-var 'kp6-variable-exists :name name))
	(setf (kp6-lookup package-object name) (make-kp6-cell (kp6-default (car name))))))
    (lookup-package-variable (name &optional (package ,package-var))
     (let ((value (kp6-lookup (kp6-find-package ,interpreter-var package) name)))
       (cond
	 (value (kp6-cell-value value))
	 (t
	  (kp6-warn 'kp6-package-variable-not-found :name name :package package)
	  (kp6-default (car name))))))
    (lookup-package-variable/c (name &optional (package ,package-var))
     (let ((value (kp6-lookup (kp6-find-package ,interpreter-var package) name)))
       (cond
	 (value value)
	 (t
	  (kp6-warn 'kp6-package-variable-not-found :name name :package package)
	  (make-kp6-cell (kp6-default (car name)))))))
    (set-package-variable (name value &optional (package ,package-var))
     (setf (kp6-lookup (kp6-find-package ,interpreter-var package) name) (make-kp6-cell value)))
    (set-package-variable/c (name value &optional (package ,package-var))
     (setf (kp6-lookup (kp6-find-package ,interpreter-var package) name) value))))

(macrolet ((define-stub-function (name)
	       `(defun ,name (&rest rest) (declare (ignore rest)) (error "~S is just a stub function!" ',name))))
  (define-stub-function enclosing-package)
  (define-stub-function define-package-variable)
  (define-stub-function set-package-variable)
  (define-stub-function set-package-variable/c)
  (define-stub-function lookup-package-variable)
  (define-stub-function lookup-package-variable/c))
