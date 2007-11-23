(in-package #:kp6-lisp)

(defun kp6-generate-variable (sigil name)
  (cons (find-symbol sigil 'kp6-lisp) name))

(dolist (sigil '($ @ % & |::|))
  (intern (string sigil) 'kp6-lisp))

(defgeneric kp6-default (sigil)
  (:documentation "Generates a default value for the given sigil."))

(defmethod kp6-default ((sigil (eql '$)))
  (make-instance 'kp6-Undef))

(defmethod kp6-default ((sigil (eql '@)))
  (make-instance 'kp6-Array))

(defmethod kp6-default ((sigil (eql '%)))
  (make-instance 'kp6-Hash))

(defmethod kp6-default ((sigil (eql '&)))
  (make-instance 'kp6-Undef))

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
