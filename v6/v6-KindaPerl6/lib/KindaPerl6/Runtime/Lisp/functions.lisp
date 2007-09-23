(in-package #:kp6-cl)

(defun kp6-normalize-function-name (name)
  (concatenate 'string "Code_" name))

(defun kp6-find-function (name &key (package "GLOBAL"))
  (let ((package-object (kp6-find-package package)))
    (unless (typep package-object 'kp6-Package)
      (error "The ~A package does not exist." package))
    (kp6-lookup package-object name)))

(defun kp6-apply-function (function args &key (package "GLOBAL"))
  (apply (kp6-value function) args))
