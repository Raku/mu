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
