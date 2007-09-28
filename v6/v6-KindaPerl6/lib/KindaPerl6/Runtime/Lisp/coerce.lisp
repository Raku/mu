(in-package #:kp6-cl)

(defgeneric kp6-coerce (object class &key &allow-other-keys)
  (:documentation "Convert a Lisp object from one class to another."))

(defmethod kp6-coerce ((object string) (class (eql 'integer)) &key (start 0) end (radix 10) junk-allowed)
  (parse-integer object :start start :end end :radix radix :junk-allowed junk-allowed))

(defmethod kp6-coerce ((object number) (class (eql 'number)) &key)
  object)

(defmethod kp6-coerce ((object number) (class (eql 'kp6-Num)) &key)
  (cl->perl object))

(defmethod kp6-coerce ((object number) (class (eql 'string)) &key)
  (format nil "~d" object))

(defmethod kp6-coerce ((object number) (class (eql 'kp6-Str)) &key)
  (cl->perl (kp6-coerce object 'string)))

(defmethod kp6-coerce ((object kp6-Num) (class (eql 'number)) &key)
  (perl->cl object))

(defmethod kp6-coerce ((object kp6-Num) (class (eql 'kp6-Num)) &key)
  object)

(defmethod kp6-coerce ((object kp6-Num) (class (eql 'string)) &key)
  (kp6-coerce (perl->cl object) class))

(defmethod kp6-coerce ((object kp6-Num) (class (eql 'kp6-Str)) &key)
  (kp6-coerce (perl->cl object) class))

(defmethod kp6-coerce ((object string) (class (eql 'number)) &key)
  (or (parse-integer object :junk-allowed t) 0))

(defmethod kp6-coerce ((object string) (class (eql 'kp6-Num)) &key)
  (cl->perl (kp6-coerce object 'number)))

(defmethod kp6-coerce ((object string) (class (eql 'string)) &key)
  object)

(defmethod kp6-coerce ((object string) (class (eql 'kp6-Str)) &key)
  (cl->perl object))

(defmethod kp6-coerce ((object kp6-Str) (class (eql 'number)) &key)
  (kp6-coerce (perl->cl object) class))

(defmethod kp6-coerce ((object kp6-Str) (class (eql 'kp6-Num)) &key)
  (kp6-coerce (perl->cl object) class))

(defmethod kp6-coerce ((object kp6-Str) (class (eql 'string)) &key)
  (perl->cl object))

(defmethod kp6-coerce ((object kp6-Str) (class (eql 'kp6-Str)) &key)
  object)

(defmethod kp6-coerce ((object kp6-Bit) (class (eql 'kp6-Num)) &key)
  (cl->perl (if (kp6-true object) 1 0)))

(defmethod kp6-coerce ((object kp6-Bit) (class (eql 'kp6-Str)) &key)
  (cl->perl (if (kp6-true object) "Bool::True" "Bool::False")))

(defmethod kp6-coerce ((object (eql t)) (class (eql 'kp6-Bit)) &key)
  (cl->perl 'true))

(defmethod kp6-coerce ((object (eql nil)) (class (eql 'kp6-Bit)) &key)
  (cl->perl 'false))
