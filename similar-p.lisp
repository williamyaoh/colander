(in-package #:colander/similar-p)

(defgeneric similar-p (obj1 obj2)
  (:documentation
   "I don't know why EQUAL isn't a generic function.")
  (:method (obj1 obj2)
    (declare (ignore obj1 obj2))
    nil))
