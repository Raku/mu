(defclass Code ()
  ((value :initarg :value)))
  
(defmethod APPLY ((c Code))
    ...
)