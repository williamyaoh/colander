(in-package #:colander/utils)

(defmacro eval-now (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))
