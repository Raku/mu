(in-package #:kp6-cl)

(let ((packages (make-instance 'kp6-Hash)))
  (defun kp6-create-package (name)
    (when (kp6-LOOKUP packages name) (warn "~S already exists in package table!" name))
    (kp6-STORE packages name (make-instance 'kp6-Package)))

  (defun kp6-find-package (name)
    (kp6-LOOKUP packages name))

  (defun kp6-remove-package (name)
    (kp6-DELETE packages name)))

(defun is-kp6-package (object)
  (typep object 'kp6-Package))

(kp6-create-package "GLOBAL")

;;; Defines a new
(defmacro define-kp6-function ((name &key (package "GLOBAL") returns) params &body body)
  "Define a new function in Perl 6 land, within the given package.
RETURNS may be specified to unconditionally return a value \(it will
be passed through CL->PERL first; for example, :RETURNS 'TRUE will
result in \(MAKE-INSTANCE 'KP6-BIT :VALUE 1\)\)."
  `(progn
    (when (null (kp6-find-package ,package))
      (kp6-create-package ,package))
    (kp6-store (kp6-find-package ,package)
     (concatenate 'string "Code_" ,name)
     (make-instance 'kp6-Code
      :value #'(lambda ,params
		 ,@body
		 ,@(when (not (null returns))
			 (list `(cl->perl ,returns))))))))

(define-kp6-function ("say" :returns 'true) (&rest strs)
  (format t "~{~A~}~%" (mapcar #'kp6-value strs)))

(define-kp6-function ("elems") (array)
  (assert (typep array 'kp6-Array))
  (length (kp6-value array)))
