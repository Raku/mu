(in-package #:kp6-cl)

(defclass kp6-Code (kp6-Value)
  ((signature :accessor kp6-signature :initarg :signature)))

(defclass kp6-argument (kp6-Object)
  ((type :accessor kp6-type :initarg :type)
   (value :accessor kp6-value :initarg :value)))

(defmethod print-object ((object kp6-argument) s)
  (format s "[~A argument: ~A]" (kp6-type object) (perl->display (kp6-cell-value (kp6-value object)))))

(defun make-kp6-argument (type value)
  (make-instance 'kp6-argument :type type :value value))

(define-condition kp6-bad-arguments (kp6-error)
  ((signature :accessor kp6-signature :initarg :signature)
   (arguments :accessor kp6-arguments :initarg :arguments))
  (:report (lambda (c s)
	     (write-string (kp6-prefixed-error-message c "Bad arguments.  Expected: ~A~%Got: ~A" (kp6-signature c) (kp6-arguments c)) s))))

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

(defmacro with-kp6-arguments ((interpreter signature rest) &body body)
  (with-unique-names (sig-positional sig-optional sig-named sig-array sig-hash sig-block arguments item type value result found name)
    `(with-kp6-pad (,interpreter)
      (let* ((,arguments ,rest)
	     (,result (kp6-make-argument-list ,signature))
	     (,sig-positional (getf ,result :positional))
	     (,sig-optional (getf ,result :optional))
	     (,sig-named (getf ,result :named))
	     (,sig-array (getf ,result :slurpy-array))
	     (,sig-hash (getf ,result :slurpy-hash))
	     (,sig-block (getf ,result :slurpy-block)))
	(flet ((bad-args () (kp6-error ,interpreter 'kp6-bad-arguments :signature ,signature :arguments ,arguments)))
	  (dolist (,item ,arguments)
	    (let ((,type (kp6-type ,item))
		  (,value (kp6-value ,item)))
	      (ecase ,type
		(positional (cond
			      ,@(mapcar #'(lambda (x)
					    `((defined (kp6-next-unbound-argument ,x)) (setf (kp6-next-unbound-argument ,x) ,value))) (list sig-positional sig-optional))
			      ((defined ,sig-array) (push ,value (cdr ,sig-array)))
			      (t (bad-args))))
		(named (let* ((,name (kp6-name ,value))
			      (,found (kp6-find-argument ,sig-named ,name)))
			 (cond
			   ((defined ,found) (setf (kp6-argument ,sig-named ,name) ,value))
			   ((defined ,sig-named) (setf (gethash ,name ,sig-named) ,value))
			   (t (bad-args)))))
		(block (setf (cdr ,sig-block) ,value)))))
	  (unless (every #'argument-bound ,sig-positional)
	    (bad-args))
	  ,@(mapcar
	     #'(lambda (x)
		 `(dolist (,item ,x)
		   (define-lexical-variable (car ,item))
		   (set-lexical-variable/c (car ,item) (cdr ,item))))
	     (list sig-positional sig-optional sig-named))
	  (when (defined ,sig-array)
	    (define-lexical-variable (car ,sig-array))
	    (set-lexical-variable (car ,sig-array) (make-instance 'kp6-array :value (nreverse (cdr ,sig-array)))))
	  (when (defined ,sig-hash)
	    (define-lexical-variable (car ,sig-hash))
	    (set-lexical-variable (car ,sig-hash) (make-instance
						   'kp6-hash
						   :value (cdr ,sig-hash)))))
	,@body))))

(defun flatten-arguments (arguments)
  (reduce #'append arguments))

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
