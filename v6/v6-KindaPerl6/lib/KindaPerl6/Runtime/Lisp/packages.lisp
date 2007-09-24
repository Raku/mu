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

(defvar *kp6-current-package*)

(defmacro with-kp6-package (package &body body)
  `(let ((*kp6-current-package* (kp6-find-package ,package)))
    (unless (is-kp6-package *kp6-current-package*)
      (error "The package ~A does not exist." ,package))
    ,@body))

(defun kp6-define-package-variable (name &optional value type)
  (declare (ignore type))
  (kp6-store *kp6-current-package* name (or value (make-instance 'kp6-Undef))))

(defun kp6-set-package-variable (name value)
  (kp6-store *kp6-current-package* name value))
