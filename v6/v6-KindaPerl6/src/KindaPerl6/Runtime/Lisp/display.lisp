(in-package #:kp6-lisp)

(defgeneric perl->display (object &key &allow-other-keys)
  (:documentation "Format a Perl 6 object for display."))

(defmethod perl->display ((object kp6-cell) &key)
  (perl->display (kp6-cell-value object)))

(defmethod perl->display ((object kp6-Array) &key)
  (format nil "~{~A~^ ~}" (map 'list #'perl->display (kp6-value object))))

(defmethod perl->display ((object kp6-Str) &key)
  (format nil "~A" (kp6-value object)))

(defmethod perl->display ((object kp6-Num) &key)
  (format nil "~A" (kp6-value object)))

(defmethod perl->display ((object kp6-Bit) &key)
  (if (eql (kp6-value object) nil) "Bool::False" "Bool::True"))

(defmethod perl->display ((object kp6-Undef) &key)
  "")

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
