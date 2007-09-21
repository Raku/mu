(in-package #:kp6-cl)

(defgeneric cl->perl (object &key &allow-other-keys)
  (:documentation "Convert a Lisp object into a Perl 6 object."))

(defmethod cl->perl ((object string))
  (make-instance 'kp6-Str :value object))

(defmethod cl->perl ((object (eql 'true)))
  (make-instance 'kp6-Bit :value 1))

(defmethod cl->perl ((object (eql 'false)))
  (make-instance 'kp6-Bit :value 0))

(defmethod cl->perl ((object (eql 'undefined)))
  (make-instance 'kp6-Undef))

(defmethod cl->perl ((object (eql nil)))
  (make-instance 'kp6-Array))

(defgeneric perl->cl (object &key &allow-other-keys)
  (:documentation "Convert a Perl 6 object into a Lisp object."))

;;; We could simply define a method on kp6-Object rather than
;;; identical methods on kp6-Value and kp6-Container, but there is the
;;; possibility of needing different handlers for each in the future
(defmethod perl->cl ((object kp6-Value))
  (kp6-value object))

(defmethod perl->cl ((object kp6-Bit))
  (not (= (kp6-value object) 0)))

(defmethod perl->cl ((object kp6-Container))
  (kp6-value object))
