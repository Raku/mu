(dolist (file '("package"
		"Object"
		"Value" "Container"            ; Base classes
		"Undef"                        ; Undef
		"Bit" "Num" "Int" "Str" "Code" ; Values
		"Hash" "Array"                 ; Containers
		"Package"
		"GLOBAL"                       ; Functions
		))
  (load (compile-file (format nil "lib/KindaPerl6/Runtime/Lisp/~A.lisp" file))))
