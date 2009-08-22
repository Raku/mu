
;; MiniPerl6 Lisp runtime
;;
;; Author: Flavio Soibelmann Glock <fglock@gmail.com>
;;
;; Copyright 2009 by Flavio Soibelmann Glock and others.
;; 
;; This program is free software; you can redistribute it and/or modify it
;; under the same terms as Perl itself.
;; 
;; See <http://www.perl.com/perl/misc/Artistic.html>

(defpackage mp-Main
  (:use common-lisp))
(in-package mp-Main)

;; core functions

(defun sv-substr (s start count) 
  (subseq s start (+ start count)))

(if (not (ignore-errors (find-method 'sv-eq () ())))
  (defgeneric sv-eq (x y)
      (:documentation "compare string values")))
(defmethod sv-eq (x y) (eql x y))
(defmethod sv-eq ((x string) (y string)) (equal x y))
(defmethod sv-eq ((x string) (y number)) (equal x (format nil "~a" y)))
(defmethod sv-eq ((x number) (y string)) (equal (format nil "~a" x) y))

(if (not (ignore-errors (find-method 'sv-bool () ())))
  (defgeneric sv-bool (self)
      (:documentation "get a bool value")))
(defmethod sv-bool (x) x)
(defmethod sv-bool ((x number)) (not (eql x 0)))
(defmethod sv-bool ((x string)) (and (not (equal x "")) (not (equal x "0"))))

;; Grammars

(if (not (ignore-errors (find-class 'mp-MiniPerl6-Grammar)))
  (defclass mp-MiniPerl6-Grammar () ()))
(let (x)
  (setq x (make-instance 'mp-MiniPerl6-Grammar))
  (defun proto-mp-MiniPerl6-Grammar () x))

;; token <word>
(if (not (ignore-errors (find-method 'sv-word () ())))
  (defgeneric sv-word (sv-grammar &optional sv-str sv-pos)
      (:documentation "a method")))
(defmethod sv-word ((sv-grammar mp-MiniPerl6-Grammar) &optional sv-str sv-pos)
    (if (or (alphanumericp (aref sv-str sv-pos)) (char= (aref sv-str sv-pos) #\_))
         (let ((m (make-instance 'mp-MiniPerl6-Match))) 
            (setf (sv-str m) sv-str)(setf (sv-from m) sv-pos)(setf (sv-to m) (+ sv-pos 1))(setf (sv-bool m) 1) m)
         (let ((m (make-instance 'mp-MiniPerl6-Match))) 
            (setf (sv-bool m) nil) m)))


;; Match objects

(if (not (ignore-errors (find-class 'mp-MiniPerl6-Match)))
  (defclass mp-MiniPerl6-Match () (hash array)))

(defvar sv-MATCH (make-instance 'mp-MiniPerl6-Match))

(if (not (ignore-errors (find-method 'sv-hash () ())))
  (defgeneric sv-hash (self)
      (:documentation "get a hash value")))
(defmethod sv-hash ((m mp-MiniPerl6-Match)) 
  (or 
    (ignore-errors (slot-value m 'hash))
    (setf (slot-value m 'hash) (make-hash-table :test 'equal))))

(if (not (ignore-errors (find-method 'sv-array () ())))
  (defgeneric sv-array (self)
      (:documentation "get an array value")))
(defmethod sv-array ((m mp-MiniPerl6-Match)) 
  (or 
    (ignore-errors (slot-value m 'array))
    (setf (slot-value m 'array) (list nil nil nil))))
    ;; (setf (slot-value m 'array) (make-array 10 :adjustable 1))))

;; compiler utils

;; function replace-substring pasted from: 
;;   http://web.mit.edu/maxima_v5.13.0/src/maxima-5.13.0/configure.lisp
(defun replace-substring (in-string old new) 
  (let ((result ""))
    (do ((begin 0)
     (end (search old in-string) 
          (search old in-string :start2 begin)))
    ((>= begin (length in-string)) 'done)
      (if end
      (progn (setf result (concatenate 'string result 
                       (subseq in-string begin end)
                       new))
         (setf begin (+ end (length old))))
      (progn (setf result (concatenate 'string result 
                       (subseq in-string begin
                           (length in-string))))
         (setf begin (length in-string)))))
    result))

(if (not (ignore-errors (find-method 'sv-lisp_escape_string () ())))
  (defgeneric sv-hash (self)
      (:documentation "escape a lisp string value")))
(defmethod sv-lisp_escape_string ((s string)) 
    (replace-substring s "\\" "\\\\")
    (replace-substring s "\"" "\\\""))

;; function join pasted from:
;;   http://coding.derkeiler.com/Archive/Lisp/comp.lang.lisp/2007-11/msg00971.html
(defun sv-join (list &optional (delim ""))
  (with-output-to-string (s)
    (when list
        (format s "~A" (first list))
        (dolist (element (rest list))
          (format s "~A~A" delim element)))))

