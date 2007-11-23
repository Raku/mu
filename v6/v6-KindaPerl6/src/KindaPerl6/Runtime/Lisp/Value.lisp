(in-package #:kp6-lisp)

(defclass kp6-Value (kp6-Object)
  ((value
    :initarg :value
    :accessor kp6-value)))

(defmethod kp6-dispatch ((invocant kp6-Value) interpreter (method (eql :cl-landish)) &rest parameters)
  "Return a lisp object for the value, this is the slot value unless
the inheritors want to do some further munging."
  (declare (ignore parameters interpreter))
  (slot-value invocant 'value))

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
