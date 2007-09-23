(in-package #:kp6-cl)

(defun kp6-normalize-function-name (name)
  (concatenate 'string "Code_" name))

(defun kp6-find-function (name &optional (package "GLOBAL"))
  (let ((package-object (kp6-find-package package)))
    (unless (is-kp6-package package-object)
      (error "The ~A package does not exist." package))
    (kp6-lookup package-object name)))

(defun kp6-apply-function (obj args &optional (package "GLOBAL"))
  (if (typep obj 'function)
      (apply obj args)
      (let ((function (kp6-find-function obj package)))
	(unless (is-kp6-code function)
	  (error "~A is not a function in package ~A." obj package))
	(apply (kp6-value function) args))))
