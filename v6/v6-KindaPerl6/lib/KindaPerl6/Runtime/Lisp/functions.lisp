(in-package #:kp6-cl)

(defun kp6-normalize-function-name (name)
  (concatenate 'string "Code_" name))

(defun kp6-find-function (name &key (package "GLOBAL"))
  (let ((package-object (kp6-find-package package)))
    (unless (is-kp6-package package-object)
      (error "The ~A package does not exist." package))
    (kp6-lookup package-object name)))

(defun kp6-apply-function (name args &key (package "GLOBAL"))
  (let ((function (kp6-find-function name :package package)))
    (unless (is-kp6-code function)
      (error "~A is not a function in package ~A." name package))
    (apply (kp6-value function) args)))