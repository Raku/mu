(in-package #:kp6-cl)

(defvar +special-unbound-value+ (gensym))

(defclass kp6-signature (kp6-Object)
  ((invocant :accessor kp6-invocant :initarg :invocant :initform nil)
   (return :accessor kp6-return :initarg :return :initform nil)
   (positional :accessor kp6-positional :initarg :positional :initform nil)
   (optional :accessor kp6-optional :initarg :optional :initform nil)
   (named :accessor kp6-named :initarg :named :initform nil)
   (slurpy-array :accessor kp6-slurpy-array :initarg :slurpy-array :initform nil)
   (slurpy-hash :accessor kp6-slurpy-hash :initarg :slurpy-hash :initform nil)
   (slurpy-block :accessor kp6-slurpy-block :initarg :slurpy-block :initform nil)))

(defmethod print-object ((object kp6-signature) stream)
  (format stream "[Signature")
  (dolist (slot '(invocant return positional optional named slurpy-array slurpy-hash slurpy-block))
    (format stream " [~S: ~S]" slot (slot-value object slot)))
  (format stream "]"))

(defun kp6-sig-item (name &optional value)
  (if value
      (cons name value)
      name))

(defgeneric kp6-make-argument-list (signature &key &allow-other-keys)
  (:method ((signature kp6-signature) &key)
    (with-slots (slurpy-array) signature
      (let ((slots '(positional named)))
	(append (loop
		 for slot in slots
		 append (list (intern (symbol-name slot) 'keyword) (mapcar #'(lambda (item) (cons item +special-unbound-value+)) (slot-value signature slot))))
		(list :optional (mapcar #'(lambda (item) (cons item (cdr item))) (kp6-optional signature))
		      :slurpy-array (when slurpy-array (cons slurpy-array (list)))
		      :slurpy-hash nil
		      :slurpy-block nil))))))

(defun argument-bound (argument)
  (not (eql +special-unbound-value+ (cdr argument))))

(defun (setf kp6-argument) (value arguments name)
  (let ((item (kp6-find-argument arguments name)))
    (when (null item)
      (error "Couldn't find argument: ~S" name))
    (setf (cdr item) value)))

(defun kp6-next-unbound-argument (arguments)
  (find +special-unbound-value+ arguments :key #'cdr))

(defun (setf kp6-next-unbound-argument) (new arguments)
  (setf (cdr (kp6-next-unbound-argument arguments)) new))

(defun kp6-find-argument (arguments name)
  (find name arguments :key #'car))
