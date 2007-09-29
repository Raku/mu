(with-compilation-unit ()
  (dolist (file '("defpackage"
		  "util" "var"
		  "Interpreter"
		  "error"
		  "Object" "Cell"
		  "Value" "Container"	; Base classes
		  "Undef"		; Undef
		  "Bit" "Num" "Int" "Str" "Code" ; Values
		  "Hash" "Array"	; Containers
		  "Package" "Pad"
		  "foreign"             ; Utilities
		  "display" "coerce"    ; Utilities
		  "GLOBAL"		; Functions
		  ))
    (load (format nil "lib/KindaPerl6/Runtime/Lisp/~A.lisp" file))))
