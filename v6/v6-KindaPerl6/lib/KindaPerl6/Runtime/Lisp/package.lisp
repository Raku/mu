(in-package #:cl-user)

(defpackage #:kp6-cl
  (:use #:cl)
  (:export
   #:kp6-array #:kp6-bit #:kp6-code #:kp6-hash #:kp6-int #:kp6-str #:kp6-apply ; types
   #:kp6-value ; accessors
   ))

(defpackage #:kp6-GLOBAL
  (:use #:cl #:kp6-cl)
  (:export #:say))
