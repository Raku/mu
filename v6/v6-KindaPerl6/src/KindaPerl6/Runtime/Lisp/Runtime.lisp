(with-compilation-unit ()
  (dolist (file
            `("defpackage"
              "util" "var" "user" "compat"
              "Interpreter"
              "error"
              "MOP"
              "Object" "Cell" "Signature"
              "Value" "Container"       ; Base classes
              "Undef"                   ; Undef
              "Bit" "Num" "Int" "Str" "Code" ; Values
              "Hash" "Array"            ; Containers
              "Pad" "Package"
              "foreign"                 ; Utilities
              "display"                 ; Utilities
              "GLOBAL"                  ; Functions
              ))
    (load (format nil "src/KindaPerl6/Runtime/Lisp/~A.lisp" file))))


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
