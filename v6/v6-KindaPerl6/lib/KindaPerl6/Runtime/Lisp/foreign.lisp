(in-package #:kp6-cl)

(defgeneric cl->perl (object &key &allow-other-keys)
  (:documentation "Convert a Lisp object into a Perl 6 object."))

(defmethod cl->perl ((object list) &key)
  (make-instance 'kp6-Array :value object))

(defmethod cl->perl ((object character) &key)
  (cl->perl (string object)))

(defmethod cl->perl ((object string) &key)
  (make-instance 'kp6-Str :value object))

(defmethod cl->perl ((object integer) &key)
  (make-instance 'kp6-Int :value object))

(defmethod cl->perl ((object number) &key)
  (make-instance 'kp6-Num :value object))

(defmethod cl->perl ((object (eql 'true)) &key)
  (make-instance 'kp6-Bit :value 1))

(defmethod cl->perl ((object (eql 'false)) &key)
  (make-instance 'kp6-Bit :value 0))

(defmethod cl->perl ((object (eql 'undefined)) &key)
  (make-instance 'kp6-Undef))

(defmethod cl->perl ((object (eql nil)) &key)
  (make-instance 'kp6-Array))

(defgeneric perl->cl (object &key &allow-other-keys)
  (:documentation "Convert a Perl 6 object into a Lisp object."))

;;; We could simply define a method on kp6-Object rather than
;;; identical methods on kp6-Value and kp6-Container, but there is the
;;; possibility of needing different handlers for each in the future
(defmethod perl->cl ((object kp6-Value) &key)
  (kp6-value object))

(defmethod perl->cl ((object kp6-Bit) &key)
  (not (= (kp6-value object) 0)))

(defmethod perl->cl ((object kp6-Container) &key)
  (kp6-value object))
