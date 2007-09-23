(in-package #:kp6-cl)

(defvar *kp6-packages* (make-instance 'kp6-Hash))

(defun kp6-create-package (name)
  (when (kp6-LOOKUP *kp6-packages* name)
    (warn "~S already exists in package table!" name))
  (kp6-STORE *kp6-packages* name (make-instance 'kp6-Package)))

(defun kp6-find-package (name)
  (kp6-LOOKUP *kp6-packages* name))

  ;(defun kp6-remove-package (name)
  ;  (kp6-DELETE packages name)))

(defmacro with-kp6-package (package &body body)
  (let ((package-object (kp6-find-package package)))
    `(flet ((define-variable (name type value)
		(setf (kp6-lookup ,package-object ,name) ,value)))
      ,@body)))
