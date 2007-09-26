(in-package #:kp6-cl)

(defvar *kp6-global-functions* (list))

(defgeneric kp6-initialize-interpreter (interpreter &optional package &rest rest)
  (:method ((interpreter kp6-interpreter) &optional (package "GLOBAL") &rest rest)
    (declare (ignore rest))
    (let ((global (kp6-find-package interpreter "GLOBAL")))
      (dolist (function *kp6-global-functions*)
	(kp6-store global (car function) (cdr function))))
    (setf (kp6-current-package interpreter) (or (kp6-find-package interpreter package) (kp6-create-package interpreter package)))))

(macrolet ((define-kp6-function (name-and-options params &body body)
	       "Define a new function in Perl 6 land, within the given package.
RETURNS may be specified to unconditionally return a value \(it will
be passed through CL->PERL first; for example, :RETURNS 'TRUE will
result in \(MAKE-INSTANCE 'KP6-BIT :VALUE 1\)\)."
	     (destructuring-bind (name &key returns) (if (listp name-and-options) name-and-options (list name-and-options))
	       `(push
		 (cons
		  ',(kp6-normalize-function-name name)
		  (make-instance
		   'kp6-Code
		   :value #'(lambda ,params ,@body ,@(when (defined returns) (list `(cl->perl ,returns))))))
		 *kp6-global-functions*))))

  (labels ((call-kp6-function (interpreter name args)
	     (kp6-apply-function interpreter (kp6-normalize-function-name name) args))
	   (str (object) (kp6-coerce object 'kp6-Str))
	   (perl-str->cl (object) (perl->cl (str object)))
	   (perl-string= (first second) (string= (perl-str->cl first) (perl-str->cl second)))
	   (num (object) (kp6-coerce object 'kp6-Num)))
    (define-kp6-function "elems" (interpreter array)
      (declare (ignore interpreter))
      (assert (typep array 'kp6-Array) (array))
      (length (kp6-value array)))

    (define-kp6-function "infix:<eq>" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (if (perl-string= first second)
		    'true
		    'false)))

    (define-kp6-function "infix:<ne>" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (if (perl-string= first second)
		    'false
		    'true)))

    (define-kp6-function "infix:<==>" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (if (equal (perl->cl (num first)) (perl->cl (num second)))
		    'true
		    'false)))

    (define-kp6-function "infix:<!=>" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (if (equal (perl->cl (num first)) (perl->cl (num second)))
		    'false
		    'true)))

    (define-kp6-function "infix:<<>" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (< (num (perl->cl first)) (num (perl->cl second)))))

    (define-kp6-function "infix:<>>" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (> (num (perl->cl first)) (num (perl->cl second)))))


    (define-kp6-function "infix:<<=>" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (<= (num (perl->cl first)) (num (perl->cl second)))))

    (define-kp6-function "infix:<>=>" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (>= (num (perl->cl first)) (num (perl->cl second)))))

    (define-kp6-function "infix:<<=>>" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (signum (- (num (perl->cl first)) (num (perl->cl second))))))

    (define-kp6-function "infix:<+>" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (+ (num (perl->cl first)) (num (perl->cl second)))))

    (define-kp6-function "infix:<->" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (- (num (perl->cl first)) (num (perl->cl second)))))

    (define-kp6-function "infix:<*>" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (* (num (perl->cl first)) (num (perl->cl second)))))

    (define-kp6-function "infix:</>" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (/ (num (perl->cl first)) (num (perl->cl second)))))

    (define-kp6-function "infix:<~>" (interpreter &rest strs)
      (declare (ignore interpreter))
      (cl->perl (format nil "~{~A~}" (mapcar #'perl->display strs))))

    (define-kp6-function "length" (interpreter &rest strs)
      (declare (ignore interpreter))
      (cl->perl (length (perl->cl (first strs)))))

    (define-kp6-function "infix:<&&>" (interpreter &rest operands)
      (if (null operands)
	  (cl->perl 'true)
	  (if (kp6-bit (first operands))
	      (call-kp6-function interpreter "infix:<&&>" (cdr operands))
	      (cl->perl 'false))))

    (define-kp6-function "infix:<||>" (interpreter &rest operands)
      (if (null operands)
	  (cl->perl 'false)
	  (let ((operand (first operands)))
	    (if (kp6-bit operand)
		operand
		(call-kp6-function interpreter "infix:<||>" (cdr operands))))))

    (define-kp6-function ("print" :returns 'true) (interpreter &rest strs)
      (declare (ignore interpreter))
      (format t "~{~A~}" (mapcar #'perl->display strs)))

    (define-kp6-function ("say" :returns 'true) (interpreter &rest strs)
      (call-kp6-function interpreter "print" strs)
      (terpri))

    (define-kp6-function ("warn" :returns 'true) (interpreter &rest strs)
      (warn "~A" (call-kp6-function interpreter "infix:<~>" strs)))

    (define-kp6-function "defined" (interpreter object)
      (declare (ignore interpreter))
      (not (null (kp6-value object))))

    (define-kp6-function "substr" (interpreter string offset &optional length)
      (declare (ignore interpreter))
      (assert (typep string 'kp6-Str))
      (assert (typep offset 'kp6-Int))
      (let* ((string (kp6-value string))
	     (offset (kp6-value offset))
	     (actual-length (length string))
	     (end (cond
		    (length
		     (assert (typep length 'kp6-Int))
		     (+ offset (kp6-value length)))
		    (t actual-length))))
	(assert (>= actual-length offset))
	(assert (>= actual-length end))
	(subseq string offset end)))))
