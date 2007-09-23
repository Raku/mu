(in-package #:kp6-cl)

(defun kp6-normalize-function-name (name)
  (concatenate 'string "Code_" name))

(defun kp6-find-function (name &optional (package "GLOBAL"))
  (let ((package-object (kp6-find-package package)))
    (unless (is-kp6-package package)
      (error "The ~A package does not exist." package))
    (kp6-lookup package-object name)))

(defun kp6-apply-function (function args &optional (package "GLOBAL"))
  (apply (kp6-value function) args))
