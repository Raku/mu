(in-package #:kp6-cl)

(kp6-create-package "GLOBAL")

;;; Defines a new
(defmacro define-kp6-function (name-and-options params &body body)
  "Define a new function in Perl 6 land, within the given package.
RETURNS may be specified to unconditionally return a value \(it will
be passed through CL->PERL first; for example, :RETURNS 'TRUE will
result in \(MAKE-INSTANCE 'KP6-BIT :VALUE 1\)\)."
  (destructuring-bind (name &key (package "GLOBAL") returns) (if (listp name-and-options) name-and-options (list name-and-options))
    `(progn
      (when (null (kp6-find-package ,package))
	(kp6-create-package ,package))
      (kp6-store (kp6-find-package ,package)
       ,(kp6-normalize-function-name name)
       (make-instance 'kp6-Code
	:value #'(lambda ,params
		   ,@body
		   ,@(when (not (null returns))
			   (list `(cl->perl ,returns)))))))))

(define-kp6-function "elems" (array)
  (assert (typep array 'kp6-Array) (array))
  (length (kp6-value array)))

(define-kp6-function ("print" :returns 'true) (&rest strs)
  (format t "~{~A~}" (mapcar #'perl->cl strs)))

(define-kp6-function ("say" :returns 'true) (&rest strs)
  (format t "~{~A~}~%" (mapcar #'perl->cl strs)))

(define-kp6-function ("warn" :returns 'true) (&rest strs)
  (warn "~{~A~}" (mapcar #'perl->cl strs)))

(define-kp6-function "defined" (object)
  (not (null (kp6-value object))))

(define-kp6-function "substr" (string offset &optional length)
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
    (subseq string offset end)))
