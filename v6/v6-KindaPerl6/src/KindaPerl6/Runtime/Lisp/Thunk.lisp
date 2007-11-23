(in-package #:kp6-lisp)

(defclass kp6-Thunk ()
  ((code :reader kp6-code :initarg :code)))

(defmacro kp6-delay (&body body)
  `(make-instance 'kp6-Thunk :code #'(lambda () ,@body)))

(defun kp6-force (thunk)
  (assert (typep thunk 'kp6-Thunk) (thunk) "First argument to KP6-FORCE must be a KP6-THUNK, not a ~A." (type-of thunk))
  (funcall (kp6-code thunk)))

;; AUTHORS
;;
;; The Pugs Team perl6-compiler@perl.org.
;;
;; SEE ALSO
;;
;; The Perl 6 homepage at http://dev.perl.org/perl6.
;;
;; The Pugs homepage at http://pugscode.org/.
;;
;; COPYRIGHT
;;
;; Copyright 2007 by Flavio Soibelmann Glock and others.
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the same terms as Perl itself.
;;
;; See http://www.perl.com/perl/misc/Artistic.html
