(in-package #:kp6-cl)

(defclass kp6-Hash (kp6-Container)
  ((value
    :initform (make-hash-table :test #'equal))))

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

(defmethod kp6-true ((self kp6-Hash))
  (plusp (hash-table-count (kp6-value self))))
