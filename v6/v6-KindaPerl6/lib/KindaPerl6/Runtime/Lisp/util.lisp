(in-package #:kp6-cl)

(defun interned-symbol (name)
  (intern (symbol-name name)))

(defmacro with-unique-names (vars &body body)
  `(let ,(mapcar #'(lambda (var) `(,var (gensym))) vars)
    ,@body))

(declaim (inline defined))
(defun defined (object)
  "Test OBJECT for definedness.  Returns T if OBJECT is non-NIL, NIL otherwise."
  (not (null object)))
