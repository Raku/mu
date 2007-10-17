(in-package #:kp6-lisp)

(defclass kp6-Str (kp6-Value)
  ())

(defmethod kp6-dispatch ((invocant kp6-Str) interpreter (method (eql :str)) &rest parameters)
  "Stringify the uh.. string"
  (declare (ignore parameters interpreter))
  invocant)

(defmethod kp6-dispatch ((invocant kp6-Str) interpreter (method (eql :cl-landish)) &rest parameters)
  "Stringify the uh.. string"
  (declare (ignore parameters interpreter))
  (slot-value invocant 'value))

(defmethod kp6-dispatch ((invocant kp6-Str) interpreter (method (eql :true)) &rest parameters)
  "Is the string true? \"\" and \"0\" are false for now"
  (declare (ignore parameters))
  (let* ((p6-str (kp6-dispatch invocant interpreter :str))
         (cl-str (kp6-dispatch p6-str interpreter :cl-landish)))
    (make-instance 'kp6-Bit :value
                   ;; FIXME: Is this right? Should it be cast to int?
                   ;; Something else?
                   (or (string/= cl-str "")
                       (string/= cl-str "0")))))
