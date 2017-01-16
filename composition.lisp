(in-package #:colander/utils)

(defun one-of (fn &rest fns)
  (lambda (x)
    (and (some (lambda (f) (funcall f x))
               (cons fn fns))
         x)))

(defun all-of (fn &rest fns)
  (lambda (x)
    (and (every (lambda (f) (funcall f x))
                (cons fn fns))
         x)))
