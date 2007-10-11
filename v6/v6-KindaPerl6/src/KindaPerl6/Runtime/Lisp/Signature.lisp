(in-package #:kp6-lisp)

(defvar +special-unbound-value+ (gensym))

(defclass kp6-signature (kp6-Object)
  ((invocant :accessor kp6-invocant :initarg :invocant)
   (return :accessor kp6-return :initarg :return)
   (positional :accessor kp6-positional :initarg :positional)
   (optional :accessor kp6-optional :initarg :optional)
   (named :accessor kp6-named :initarg :named)
   (slurpy-array :accessor kp6-slurpy-array :initarg :slurpy-array)
   (slurpy-hash :accessor kp6-slurpy-hash :initarg :slurpy-hash)
   (slurpy-block :accessor kp6-slurpy-block :initarg :slurpy-block)))

(defmethod print-object ((object kp6-signature) stream)
  (format stream "[Signature")
  (dolist (slot '(invocant return positional optional named slurpy-array slurpy-hash slurpy-block))
    (when (slot-boundp object slot)
      (format stream " [~S: ~S]" slot (slot-value object slot))))
  (format stream "]"))

(defclass kp6-parameter ()
  ((value :accessor kp6-value :initarg :value :initform +special-unbound-value+)))

(defclass kp6-positional-parameter (kp6-parameter)
  ())

(defclass kp6-named-parameter (kp6-parameter)
  ((name :accessor kp6-name :initarg :name)))

(defclass kp6-block-parameter (kp6-parameter)
  ())

(defmethod print-object ((object kp6-parameter) stream)
  (format stream "[Argument")
  (when (slot-boundp object 'value)
    (format stream " ~S" (kp6-value object)))
  (format stream "]"))

(defmethod print-object ((object kp6-named-parameter) stream)
  (format stream "[Named argument")
  (dolist (slot '(name value))
    (when (slot-boundp object slot)
      (format stream " [~A: ~S]" slot (slot-value object slot))))
  (format stream "]"))

(define-condition kp6-bad-arguments (kp6-error)
  ((signature :accessor kp6-signature :initarg :signature)
   (arguments :accessor kp6-arguments :initarg :arguments))
  (:report (lambda (c s)
	     (write-string (kp6-prefixed-error-message c "Bad arguments.  Expected: ~A~%Got: ~A" (kp6-signature c) (kp6-arguments c)) s))))

(defgeneric signature->arguments (signature &key &allow-other-keys)
  (:method ((signature kp6-signature) &key)
    (let ((temp (make-instance 'kp6-signature)))
      (flet ((blank-parameter (parameter) (make-instance 'kp6-named-parameter :name (kp6-name parameter))))
	(dolist (slot '(positional optional named))
	  (when (slot-boundp signature slot)
	    (setf (slot-value temp slot) (mapcar
					  #'blank-parameter
					  (slot-value signature slot)))))
	(with-accessors ((invocant kp6-invocant) (return kp6-return) (slurpy-array kp6-slurpy-array) (slurpy-hash kp6-slurpy-hash) (slurpy-block kp6-slurpy-block)) temp
			(when (slot-boundp signature 'invocant)
			  (setf invocant (mapcar #'blank-parameter (slot-value signature 'invocant))))
			(when (slot-boundp signature 'return)
			  (setf return (slot-value signature 'return)))
			(when (slot-boundp signature 'slurpy-array)
			  (setf slurpy-array (blank-parameter (slot-value signature 'slurpy-array))))
			(when (slot-boundp signature 'slurpy-hash)
			  (setf slurpy-hash (blank-parameter (slot-value signature 'slurpy-hash))))
			(when (slot-boundp signature 'slurpy-block)
			  (setf slurpy-block (blank-parameter (slot-value signature 'slurpy-block))))))
      temp)))

(defmacro with-kp6-arguments ((interpreter signature rest) &body body)
  (with-unique-names (result positional optional named slurpy-array slurpy-hash slurpy-block error-report)
    `(with-kp6-pad (,interpreter)
      (flet ((,error-report () (kp6-error ,interpreter 'kp6-bad-arguments :signature ,signature :arguments ,rest)))
	(let ((,result (signature->arguments ,signature)))
	  (with-slots ((,positional positional) (,optional optional) (,named named) (,slurpy-array slurpy-array) (,slurpy-hash slurpy-hash) (,slurpy-block slurpy-block)) ,result
	    ,(collect-arguments error-report result rest positional optional named slurpy-array slurpy-hash slurpy-block)
	    ,(bind-arguments result)
	    ,@body))))))

(defun collect-arguments (error-report result arguments positional optional named slurpy-array slurpy-hash slurpy-block)
  (declare (ignore slurpy-block))
  (with-unique-names (item name)
    `(progn
      (dolist (,item ,arguments)
	(with-slots (value) ,item
	  (etypecase ,item
	    (kp6-positional-parameter
	     (cond
	       ((kp6-next-unbound-argument ,positional) (setf (kp6-next-unbound-argument ,positional) value))
	       ((and (slot-boundp ,result 'optional) (kp6-next-unbound-argument ,optional)) (setf (kp6-next-unbound-argument ,optional) value))
	       ((slot-boundp ,result 'slurpy-array)
		(unless (listp (kp6-value ,slurpy-array))
		  (setf (kp6-value ,slurpy-array) (list)))
		(push value (kp6-value ,slurpy-array)))
	       (t (,error-report))))
	    (kp6-named-parameter
	     (let ((,name (kp6-name ,item)))
	       (cond
		 ((kp6-find-argument ,named ,name) (setf (kp6-find-argument ,named ,name) value))
		 ((slot-boundp ,result 'slurpy-hash) (setf (gethash ,name ,slurpy-hash) value))
		 (t (,error-report)))))))))))

(defun bind-arguments (result)
  (flet ((bind-list (slot)
	   (with-unique-names (item)
	     `(when (slot-boundp ,result ',slot)
	       (dolist (,item ,slot)
		 (define-lexical-variable (kp6-name ,item))
		 (set-lexical-variable/c (kp6-name ,item) (if (slot-boundp ,item 'value) (kp6-value ,item) (make-kp6-cell (kp6-default (car (kp6-name ,item)))))))))))
    `(with-slots (positional optional named slurpy-array slurpy-hash slurpy-block) ,result
      ,@(mapcar #'bind-list '(positional optional named))
      (with-slots (slurpy-hash slurpy-array) ,result
	(when (slot-boundp ,result 'slurpy-hash)
	  (with-slots (name value) slurpy-hash
	    (define-lexical-variable name)
	    (set-lexical-variable/c name (make-kp6-cell (make-instance 'kp6-Hash :value (if (slot-boundp slurpy-hash 'value) value (make-hash-table)))))))
	(when (slot-boundp ,result 'slurpy-array)
	  (define-lexical-variable (kp6-name slurpy-array))
	  (set-lexical-variable/c (kp6-name slurpy-array) (make-kp6-cell (make-instance 'kp6-Array :value (if (slot-boundp slurpy-array 'value) (nreverse (kp6-value slurpy-array)) (make-array 1))))))))))

(defun kp6-next-unbound-argument (arguments)
  (find-if #'(lambda (x) (eql (kp6-value x) +special-unbound-value+)) arguments))

(defun (setf kp6-next-unbound-argument) (value arguments)
  (setf (kp6-value (kp6-next-unbound-argument arguments)) value))

(defun kp6-find-argument (arguments name)
  (find-if #'(lambda (x) (eql (kp6-name x) name)) arguments))

(defun (setf kp6-find-argument) (value arguments name)
  (setf (kp6-value (kp6-find-argument arguments name)) value))
