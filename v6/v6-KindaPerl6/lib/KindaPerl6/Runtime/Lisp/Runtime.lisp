(dolist (file '("package"
		"Bit" "Int" "Str" "Code" ; Values
		"Hash" "Array"           ; Containers
		"GLOBAL"                 ; Functions
		))
  (load (compile-file (format nil "lib/KindaPerl6/Runtime/Lisp/~A.lisp" file))))
