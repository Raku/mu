(in-package #:kp6-lisp)

(defclass kp6-Int (kp6-Num)
  ())

(defmethod kp6-dispatch ((invocant kp6-Int) interpreter (method (eql :Str)) &rest parameters)
  "Stringify the Int with `format'"
  (declare (ignore parameters interpreter))
  (make-instance 'kp6-Str :value (format nil "~d" (slot-value invocant 'value))))

(defmethod kp6-dispatch ((invocant kp6-Int) interpreter (method (eql :Int)) &rest parameters)
  "Intify the Int"
  (declare (ignore parameters interpreter))
  invocant)

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
