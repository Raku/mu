(defpackage Main)

(defun Main::substr (s start count) 
  (subseq s start (+ start count)))

(defun Main::bool (x)
    (if (stringp x) 
        (not (or (equal x "") (equal x "0"))) 
        (if (numberp x)    
            (not (equal x 0)) 
            x )))

