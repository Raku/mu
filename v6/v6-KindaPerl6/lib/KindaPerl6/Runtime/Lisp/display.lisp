(in-package #:kp6-lisp)

(defgeneric perl->display (object &key &allow-other-keys)
  (:documentation "Format a Perl 6 object for display."))

(defmethod perl->display ((object kp6-cell) &key)
  (perl->display (kp6-cell-value object)))

(defmethod perl->display ((object kp6-Array) &key)
  (format nil "~{~A~^ ~}" (mapcar #'perl->display (kp6-value object))))

(defmethod perl->display ((object kp6-Str) &key)
  (format nil "~A" (kp6-value object)))

(defmethod perl->display ((object kp6-Num) &key)
  (format nil "~A" (kp6-value object)))

(defmethod perl->display ((object kp6-Bit) &key)
  (if (eql (kp6-value object) nil) "Bool::False" "Bool::True"))

(defmethod perl->display ((object kp6-Undef) &key)
  "")
