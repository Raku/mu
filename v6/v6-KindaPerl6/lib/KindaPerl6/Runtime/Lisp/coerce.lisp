(in-package #:kp6-cl)

(defgeneric kp6-coerce (object class &key &allow-other-keys)
  (:documentation "Convert a Lisp object from one class to another."))

(defmethod kp6-coerce ((object string) (class (eql 'integer)) &key (start 0) end (radix 10) junk-allowed)
  (parse-integer object :start start :end end :radix radix :junk-allowed junk-allowed))

(defmethod kp6-coerce ((object kp6-Num) (class (eql 'kp6-Str)) &key)
  (cl->perl (format nil "~d" (kp6-value object))))

(defmethod kp6-coerce ((object kp6-Num) (class (eql 'kp6-Num)) &key)
  object)

(defmethod kp6-coerce ((object kp6-Str) (class (eql 'kp6-Num)) &key)
  (cl->perl (or (parse-integer object :junk-allowed t) 0)))

(defmethod kp6-coerce ((object kp6-Str) (class (eql 'kp6-Str)) &key)
  object)
