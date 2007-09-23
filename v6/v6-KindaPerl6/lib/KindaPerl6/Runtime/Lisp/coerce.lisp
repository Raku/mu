(in-package #:kp6-cl)

(defgeneric kp6-coerce (object class &key &allow-other-keys)
  (:documentation "Convert a Lisp object from one class to another."))

(defmethod kp6-coerce ((object string) (class (eql 'integer)) &key (start 0) end (radix 10) junk-allowed)
  (parse-integer string :start start :end end :radix radix :junk-allowed junk-allowed))
