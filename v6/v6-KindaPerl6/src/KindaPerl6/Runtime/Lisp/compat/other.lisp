(in-package #:kp6-lisp)

(define-condition kp6-implementation-not-supported (kp6-error)
  ((feature :accessor kp6-feature :initarg :feature))
  (:report (lambda (c s)
             (write-string (kp6-prefixed-error-message "Your implementation does not support this feature: ~A" (kp6-feature c)) s))))

(defun kp6-check-implementation-compatibility ()
  (warn "Unknown implementation.  Certain features may not work as expected."))

(defmethod kp6-quit ((interpreter kp6-interpreter) &key message code)
  (kp6-error interpreter 'kp6-implementation-not-supported :feature "graceful exits"))
