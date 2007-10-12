(in-package #:kp6-lisp)

;; The test function is #'equal which works because we only support
;; string keys at the moment. (Does p6 even support complex types as
;; keys?)
;;
;; Due to this hack .pairs, .keys and other methods that return keys
;; need to construct a kp6-Str object again

(defclass kp6-Hash (kp6-Container)
  ((value
    :initform (make-hash-table :test #'equal))))

;; XXX: Everything below which is not (defmethod kp6-dispatch) is
;; *only used internally*. Internals (like Pad) should probably be
;; rewritten to use the new stuff or we should move this somewhere
;; else if we don't want to constantly convert back-and-forth between
;; cl and perl in the internals.

(defmethod kp6-store ((self kp6-Hash) key value &key)
  "Stores a key-value pair in the hash"
  (let ((hash (slot-value self 'value)))
    (setf (gethash key hash) value)
    hash))

(defmethod kp6-lookup ((self kp6-Hash) key &key)
  "Looks up a value in the hash by key"
  (let* ((hash (kp6-value self))
	 (entry (gethash key hash)))
    entry))

(defmethod kp6-exists ((self kp6-Hash) key &key)
  "Test whether there exists an entry in SELF with key KEY."
  (nth-value 1 (gethash key (kp6-value self))))

(defmethod kp6-delete ((self kp6-Hash) key &key)
  "Deletes a key-value pair from the hash given a key"
  (make-instance 'kp6-Bit :value
    (let ((hash (kp6-value self)))
      (remhash key hash))))

(defmethod kp6-clear ((self kp6-Hash) &key)
  "Empties the hash"
  (let ((hash (kp6-value self)))
    (clrhash hash))
  ; XXX: Just return true for now?
  (make-instance 'kp6-Bit :value 1))

(defmethod kp6-pairs ((self kp6-Hash) &key)
  "Returns an Array of key-value pairs in the hash in `maphash' order"
  (make-instance 'kp6-Array :value 
    (let ((hash (kp6-value self))
          (values))
      (maphash #'(lambda (key val)
                   (push val values)
                   (push key values))
               hash)
      values)))

(defmethod kp6-elems ((self kp6-Hash) &key)
  "Returns the number of elements in the hash"
  (make-instance 'kp6-Int :value 
    (hash-table-count (kp6-value self))))

(defmethod kp6-dispatch ((invocant kp6-Hash) interpreter (method (eql :true)) &rest parameters)
  "A hash is true if it contains nonzero entries"
  (declare (ignore parameters))
  (make-instance 'kp6-Bit :value (plusp (hash-table-count (kp6-dispatch invocant interpreter :cl-landish)))))

(defmethod kp6-dispatch ((invocant kp6-Hash) interpreter (method (eql :cl-landish)) &rest parameters)
  "Return a lisp object for the Hash"
  (declare (ignore parameters interpreter))
  (slot-value invocant 'value))

(defmethod kp6-dispatch ((invocant kp6-Hash) interpreter (method (eql :store)) &rest parameters)
  "Stores a key-value pair in the hash"
  (declare (ignore interpreter))
  (assert (= 2 (length parameters)))
  (let ((key (perl->cl (elt parameters 0)))
	(value (elt parameters 1)))
    (let ((hash (slot-value invocant 'value)))
      (setf (gethash key hash) value)
      hash)))

(defmethod kp6-dispatch ((invocant kp6-Hash) interpreter (method (eql :lookup)) &rest parameters)
  "Looks up a value in the hash by key"
  (declare (ignore interpreter))
  (assert (= 1 (length parameters)))
  (let ((key (perl->cl (elt parameters 0)))
	(hash (slot-value invocant 'value)))
    (let ((value (gethash key hash)))
      (if value value (make-instance 'kp6-Undef)))))

(defmethod kp6-dispatch ((invocant kp6-Hash) interpreter (method (eql :elems)) &rest parameters)
  "Returns the number of elements in the hash"
  (declare (ignore parameters interpreter))
  (make-instance 'kp6-Int
     :value (hash-table-count (slot-value invocant 'value))))

(defmethod kp6-dispatch ((invocant kp6-Hash) interpreter (method (eql :keys)) &rest parameters)
  "Returns a list of keys in the hash in `maphash' order"
  (declare (ignore parameters))
  (let ((hash (slot-value invocant 'value))
	(values (make-instance 'kp6-array)))
    (maphash #'(lambda (key val)
		 (declare (ignore val))
		 (kp6-dispatch values interpreter :push (make-instance 'kp6-Str :value key)))
	     hash)
    values))

(defmethod kp6-dispatch ((invocant kp6-Hash) interpreter (method (eql :values)) &rest parameters)
  "Returns a list of values in the hash in `maphash' order"
  (declare (ignore parameters))
  (let ((hash (slot-value invocant 'value))
	(values (make-instance 'kp6-array)))
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (kp6-dispatch values interpreter :push val))
	     hash)
    values))

(defmethod kp6-dispatch ((invocant kp6-Hash) interpreter (method (eql :exists)) &rest parameters)
  "Test whether an entry exists"
  (declare (ignore interpreter))
  (let ((key (perl->cl (elt parameters 0))))
    (make-instance 'kp6-Bit :value
                   (not (null (nth-value 1 (gethash key (slot-value invocant 'value))))))))

(defmethod kp6-dispatch ((invocant kp6-Hash) interpreter (method (eql :delete)) &rest parameters)
  "Deletes a key-value pair from the hash given a key"
  (let ((hash (slot-value invocant 'value))
        (key (perl->cl (elt parameters 0))))
    (make-instance 'kp6-Bit :value (remhash key hash))))

(defmethod kp6-dispatch ((invocant kp6-Hash) interpreter (method (eql :clear)) &rest parameters)
  "Empties the hash"
  (declare (ignore parameters interpreter))
  (clrhash (slot-value invocant 'value))
  ;; XXX: Always return true?
  (make-instance 'kp6-Bit :value t))

(defmethod kp6-dispatch ((invocant kp6-Hash) interpreter (method (eql :pairs)) &rest parameters)
  "Returns an Array of key-value pairs in the hash in `maphash' order"
  (declare (ignore parameters))
  (let ((hash (slot-value invocant 'value))
	(values (make-instance 'kp6-array)))
    (maphash #'(lambda (key val)
		 (kp6-dispatch interpreter values :push val)
		 (kp6-dispatch interpreter values :push (make-instance 'kp6-str :value key)))
	     hash)
    values))
