(in-package #:colander/defschema)

(defun in-order-p (eles list)
  "Verify that the given ELES, if they appear in LIST, are in the
   specified order."
  (flet ((<* (n1 n2)
           (cond ((null n2) n1)
                 ((null n1) n2)
                 (:otherwise (and (< n1 n2) n2)))))
    (and (reduce #'<* (map 'list (lambda (ele) (position ele list)) eles)
                 :initial-value -1)
         t)))

(defun &symbolp (symb)
  (and (symbolp symb)
       (string= (symbol-name symb) "&" :end1 1)))


