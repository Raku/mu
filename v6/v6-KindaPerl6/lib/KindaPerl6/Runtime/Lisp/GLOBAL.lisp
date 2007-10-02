(in-package #:kp6-cl)

(defvar *kp6-global-functions* (list))

(defgeneric kp6-initialize-interpreter (interpreter &key &allow-other-keys)
  (:method ((interpreter kp6-interpreter) &key)
    (let ((global (kp6-find-package interpreter "GLOBAL")))
      (dolist (function *kp6-global-functions*)
	(kp6-store global (car function) (make-kp6-cell (cdr function)))))))

(macrolet ((define-kp6-function (name-and-options params &body body)
	       "Define a new function in Perl 6 land, within the given package.
RETURNS may be specified to unconditionally return a value \(it will
be passed through CL->PERL first; for example, :RETURNS 'TRUE will
result in \(MAKE-INSTANCE 'KP6-BIT :VALUE 1\)\)."
	     (destructuring-bind (name &key coerce returns) (if (listp name-and-options) name-and-options (list name-and-options))
	       (let ((declaration (list))
		     (body (copy-list body)))
		 (when (eql (caar body) 'declare)
		   (setf declaration (car body))
		   (setf body (remove declaration body)))
		 `(push
		   (cons
		    ',(kp6-normalize-function-name name)
		    (make-instance
		     'kp6-Code
		     :value #'(lambda ,params ,@(when declaration `(,declaration)) ,@(if coerce `((kp6-coerce (progn ,@body) ,coerce)) body) ,@(when (defined returns) (list `(cl->perl ,returns))))))
		   *kp6-global-functions*)))))

  (labels ((call-kp6-function (interpreter name args)
	     (kp6-apply-function interpreter (kp6-normalize-function-name name) args))
	   (str* (object) (kp6-coerce object 'string))
	   (num* (object) (kp6-coerce object 'number)))
    (define-kp6-function "elems" (interpreter array)
      (declare (ignore interpreter))
      (assert (typep array 'kp6-Array) (array))
      (length (kp6-value array)))

    (define-kp6-function ("infix:<eq>" :coerce 'kp6-Bit) (interpreter first second)
      (declare (ignore interpreter))
      (string= (str* first) (str* second)))

    (define-kp6-function ("infix:<ne>" :coerce 'kp6-Bit) (interpreter first second)
      (declare (ignore interpreter))
      (not (string= (str* first) (str* second))))

    (define-kp6-function ("infix:<==>" :coerce 'kp6-Bit) (interpreter first second)
      (declare (ignore interpreter))
      (equal (num* first) (num* second)))

    (define-kp6-function ("infix:<!=>" :coerce 'kp6-Bit) (interpreter first second)
      (declare (ignore interpreter))
      (not (equal (num* first) (num* second))))

    (define-kp6-function ("infix:<<>" :coerce 'kp6-Bit) (interpreter first second)
      (declare (ignore interpreter))
      (< (num* first) (num* second)))

    (define-kp6-function ("infix:<>>" :coerce 'kp6-Bit) (interpreter first second)
      (declare (ignore interpreter))
      (> (num* first) (num* second)))

    (define-kp6-function ("infix:<<=>" :coerce 'kp6-Bit) (interpreter first second)
      (declare (ignore interpreter))
      (<= (num* first) (num* second)))

    (define-kp6-function ("infix:<>=>" :coerce 'kp6-Bit) (interpreter first second)
      (declare (ignore interpreter))
      (>= (num* first) (num* second)))

    (define-kp6-function "infix:<<=>>" (interpreter first second)
      (declare (ignore interpreter))
      (signum (- (num* first) (num* second))))

    (define-kp6-function "infix:<+>" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (+ (num* first) (num* second))))

    (define-kp6-function "infix:<->" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (- (num* first) (num* second))))

    (define-kp6-function "infix:<*>" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (* (num* first) (num* second))))

    (define-kp6-function "infix:</>" (interpreter first second)
      (declare (ignore interpreter))
      (cl->perl (/ (num* first) (num* second))))

    (define-kp6-function "infix:<~>" (interpreter &rest strs)
      (declare (ignore interpreter))
      (cl->perl (format nil "~{~A~}" (mapcar #'perl->display strs))))

    (define-kp6-function "length" (interpreter &rest strs)
      (declare (ignore interpreter))
      (cl->perl (length (perl->cl (first strs)))))

    (define-kp6-function "infix:<&&>" (interpreter &rest operands)
      (if (null operands)
	  (cl->perl 'true)
	  (if (kp6-true (first operands))
	      (call-kp6-function interpreter "infix:<&&>" (cdr operands))
	      (cl->perl 'false))))

    (define-kp6-function "infix:<||>" (interpreter &rest operands)
      (if (null operands)
	  (cl->perl 'false)
	  (let ((operand (first operands)))
	    (if (kp6-true operand)
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
      ; XXX: cl->perl doesn't like nil/t
      (make-instance 'kp6-Bit :value (not (null (kp6-value object)))))

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
	(cl->perl (subseq string offset end))))))
