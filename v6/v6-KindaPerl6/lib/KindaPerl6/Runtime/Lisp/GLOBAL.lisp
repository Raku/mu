; lookup the &GLOBAL::say variable,
; and then store the Code object into it
(in-package #:kp6-cl)

(let ((packages (make-instance 'kp6-Hash)))
  (defun kp6-create-package (name)
    (when (kp6-LOOKUP packages name) (warn "~S already exists in package table!" name))
    (kp6-STORE packages name (make-instance 'kp6-Package)))

  (defun kp6-find-package (name)
    (kp6-LOOKUP packages name))

  (defun kp6-remove-package (name)
    (kp6-DELETE packages name)))

(kp6-create-package "GLOBAL")

(kp6-store (kp6-find-package "GLOBAL")
	   "Code_say"
	   (make-instance 'kp6-Code
			  :value #'(lambda (&rest strs)
				     (dolist (str strs)
				       (format t "~A" (kp6-value str)))
				     (terpri)
				     (cl->perl 'true))))
