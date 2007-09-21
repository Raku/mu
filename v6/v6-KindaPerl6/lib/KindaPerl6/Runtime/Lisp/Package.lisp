(in-package #:cl-user)

(defpackage #:kp6-cl
  (:use #:cl)
  (:export
   ;; Classes (types)
   #:kp6-Array #:kp6-Bit #:kp6-Code #:kp6-Hash #:kp6-Int #:kp6-Str ;#:kp6-Apply
   
   ;; Accessors
   #:kp6-value

   ; Hash.lisp
   #:kp6-STORE
   #:kp6-LOOKUP
   #:kp6-DELETE
   #:kp6-CLEAR
   #:kp6-pairs
   #:kp6-elems
   
   ))
