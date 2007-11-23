(in-package #:cl-user)

(defpackage #:kp6-lisp
  (:use #:cl)
  (:export
   ;; Classes (types)
   #:kp6-Array #:kp6-Bit #:kp6-Code #:kp6-Hash #:kp6-Int #:kp6-Str #:kp6-Package #:kp6-Object #:kp6-Value #:kp6-Num #:kp6-Container #:kp6-Undef #:kp6-Char #:kp6-Pad #:kp6-Interpreter
   #:kp6-signature #:kp-parameter #:kp6-positional-parameter #:kp6-named-parameter #:kp6-block-parameter

   #:make-kp6-sub
   #:make-kp6-cell
   #:make-kp6-argument

   ;; MOP
   #:kp6-dispatch

   ;; Accessors
   #:kp6-value
   #:kp6-pad

   #:kp6-apply-function

   ; foreign.lisp
   #:cl->perl
   #:perl->cl

   #:kp6-packages
   #:kp6-ensure-package

   #:kp6-generate-variable
   #:kp6-signal
   #:kp6-condition
   #:kp6-warn
   #:kp6-warning
   #:kp6-error
   #:kp6-not-implemented

   #:enclosing-pad
   #:outer-pad
   #:lexical-variable-exists
   #:define-lexical-variable
   #:set-lexical-variable
   #:set-lexical-variable/c
   #:lookup-lexical-variable
   #:lookup-lexical-variable/c
   #:define-our-variable

   #:enclosing-package
   #:define-package-variable
   #:set-package-variable
   #:set-package-variable/c
   #:lookup-package-variable
   #:lookup-package-variable/c

   #:with-kp6-package
   #:with-kp6-pad
   #:with-kp6-interpreter
   #:kp6-for-loop-structure

   #:quit

   #:kp6-initialize-interpreter))

(defpackage #:kp6-lisp-user
  (:use #:cl #:kp6-lisp))

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
