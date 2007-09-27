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

(defgeneric kp6-define-package-variable (interpreter package name &optional type)
  (:method ((interpreter kp6-interpreter) package name &optional type)
    (declare (ignore type))
    (kp6-store (kp6-find-package interpreter package) name (kp6-default (car name)))))

(defgeneric kp6-set-package-variable (interpreter package name value)
  (:method ((interpreter kp6-interpreter) package name value)
    (kp6-store (kp6-find-package interpreter package) name value)))

(defgeneric kp6-get-package-variable (interpreter package name)
  (:method ((interpreter kp6-interpreter) package name)
    (kp6-lookup (kp6-find-package interpreter package) name)))

(defmacro with-kp6-package ((interpreter package) &body body)
  (with-unique-names (interpreter-var package-var)
    `(let* ((,interpreter-var ,interpreter)
	    (,package-var (kp6-lookup (kp6-packages ,interpreter-var) ,package)))
      (flet ,(kp6-with-package-functions package-var interpreter-var)
	(declare (ignorable ,@(mapcar #'(lambda (name) `#',(interned-symbol name)) '(enclosing-package outer-package define-package-variable set-package-variable lookup-package-variable lookup-package-variable/p))))
	(with-kp6-pad (,interpreter-var)
	  ,@body)))))

(defun kp6-with-package-functions (package-var interpreter-var)
  (mapcar
   #'(lambda (func) `(,(interned-symbol (car func)) ,@(cdr func)))
   `((enclosing-package () ,package-var)
     (outer-package () (when (fboundp ',(interned-symbol 'enclosing-package)) (funcall #',(interned-symbol 'enclosing-package))))
     (define-package-variable (name &optional (package ,package-var) type)
	 (kp6-define-package-variable ,interpreter-var package name type))
     (set-package-variable (name value &optional (package ,package-var))
      (kp6-set-package-variable ,interpreter-var package name value))
     (lookup-package-variable (name &optional (package ,package-var))
      (kp6-get-package-variable ,interpreter-var package name)))))
