(in-package #:kp6-cl)

(defclass kp6-hash (kp6-container)
  ())

; XXX: Best way to make a constuctor in a class?
(defun kp6-Hash-new ()
  "Returns a p6-Hash instance"
  (let ((hash (make-hash-table :test #'equal)))
    ; TODO: Populate the hash with key-value pairs?
    (make-instance 'kp6-Hash :value hash)))

(defmethod STORE ((self kp6-Hash) key value)
  "Stores a key-value pair in the hash"
  (let ((hash (slot-value self 'value)))
    (setf (gethash key hash) value)
    hash))

(defmethod LOOKUP ((self kp6-Hash) key)
  "Looks up a value in the hash by key"
  (let* ((hash (slot-value self 'value))
        (entry (gethash key hash)))
    entry))

(defmethod pairs ((self kp6-Hash))
  "Returns a list of key-value pairs in the hash in undetermined order"
  (make-instance 'kp6-Array :value 
    (let ((hash (slot-value self 'value))
          (values))
      (maphash #'(lambda (key val)
                   (push key values)
                   (push val values))
               hash)
      values)))
                 
(defmethod elems ((self kp6-Hash))
  "Returns the number of elements in the hash"
  (make-instance 'kp6-Int :value 
    (hash-table-count (slot-value self 'value))))
