(in-package #:kp6-cl)

(defconstant +kp6-default-package+ "GLOBAL")

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

(defgeneric kp6-define-package-variable (interpreter name &optional value type)
  (:method ((interpreter kp6-interpreter) name &optional value type)
    (declare (ignore type))
    (kp6-store (kp6-current-package interpreter) name (or value (kp6-default (car name))))))

(defgeneric kp6-set-package-variable (interpreter name value)
  (:method ((interpreter kp6-interpreter) name value)
    (kp6-store (kp6-current-package interpreter) name value)))

(defmacro with-kp6-package ((interpreter package) &body body)
  (with-unique-names (interpreter-var package-var old)
    `(let* ((,interpreter-var ,interpreter)
	   (,package-var ,package)
	   (,old (when (slot-boundp ,interpreter-var 'current-package) (kp6-current-package ,interpreter-var))))
      (setf (kp6-current-package ,interpreter-var) (or (kp6-find-package ,interpreter-var ,package-var) (kp6-error ,interpreter-var 'kp6-package-not-found :package ,package-var)))
      (if ,old
	  (unwind-protect
	       (progn
		 ,@body)
	    (setf (kp6-current-package ,interpreter-var) ,old))
	  (progn
	    ,@body)))))
