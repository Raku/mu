(in-package #:kp6-cl)

(defclass kp6-Str (kp6-Value)
  ())

(defmethod kp6-Num ((self kp6-Str))
  "Numify the Str object"
  (let* ((values (multiple-value-list (parse-integer (kp6-value self) :junk-allowed t)))
         (int (first values))
         (pos (second values)))
    (cl->perl (if int int 0))))