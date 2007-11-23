(in-package #:kp6-lisp)

(defclass kp6-Str (kp6-Value)
  ())

(defmethod kp6-dispatch ((invocant kp6-Str) interpreter (method (eql :Str)) &rest parameters)
  "Stringify the Str"
  (declare (ignore parameters interpreter))
  invocant)

(defmethod kp6-dispatch ((invocant kp6-Str) interpreter (method (eql :Int)) &rest parameters)
  "Intify the Str with `parse-integer'"
  (declare (ignore parameters))
  (let* ((string (kp6-dispatch invocant interpreter :cl-landish))
         (start 0)
         (radix 10)
         (integer (or (parse-integer string :start start :radix radix :junk-allowed t) 0)))
    (make-instance 'kp6-Int :value integer)))

(defmethod kp6-dispatch ((invocant kp6-Str) interpreter (method (eql :cl-landish)) &rest parameters)
  "Stringify the uh.. string"
  (declare (ignore parameters interpreter))
  (slot-value invocant 'value))

(defmethod kp6-dispatch ((invocant kp6-Str) interpreter (method (eql :true)) &rest parameters)
  "Is the string true? \"\" and \"0\" are false for now"
  (declare (ignore parameters))
  (let* ((p6-str (kp6-dispatch invocant interpreter :str))
         (cl-str (kp6-dispatch p6-str interpreter :cl-landish)))
    (make-instance 'kp6-Bit :value
                   ;; FIXME: Is this right? Should it be cast to int?
                   ;; Something else?
                   (or (string/= cl-str "")
                       (string/= cl-str "0")))))


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
