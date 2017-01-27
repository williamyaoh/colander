(in-package #:colander/utils)

(defun normalize-slot-designator (slot-des)
  (etypecase slot-des
    (symbol (list slot-des slot-des))
    (cons slot-des)))

(defun normalize-slot-spec (slot-spec)
  (cond
    ((or (symbolp slot-spec)
         (and (consp slot-spec) (symbolp (car slot-spec))
              (cadr slot-spec) (symbolp (cadr slot-spec))))
     (list (normalize-slot-designator slot-spec) '()))
    ((and (consp slot-spec) (cdr slot-spec) (listp (cadr slot-spec)))
     (list (normalize-slot-designator (car slot-spec))
           (map 'list #'normalize-slot-spec (cadr slot-spec))))
    (:otherwise
     (error "Malformed slot specification: ~S" slot-spec))))

(defmacro with-nested-slots ((&rest slot-specs) obj &body body)
  "Like WITH-SLOTS, but allows for binding the slots of nested objects.

   SLOT-SPECS ::= (SPEC [SPEC...])

   SPEC
     ::= SLOT-DESIGNATOR
       | (SLOT-DESIGNATOR SLOT-SPECS)

   SLOT-DESIGNATOR
     ::= SLOT-NAME
       | (VAR-NAME SLOT-NAME)

   SLOT-NAME, VAR-NAME ::= SYMBOL"
  (if (null slot-specs)
      `(progn ,obj ,@body)
      (let ((specs (map 'list #'normalize-slot-spec slot-specs)))
        (labels ((1layer-slots (specs)
                   (if (null specs)
                       `(progn ,@body)
                       (destructuring-bind ((var-name slot-name) children) (first specs)
                         (declare (ignorable slot-name))
                         `(with-nested-slots ,children ,var-name
                            ,(1layer-slots (rest specs)))))))
          `(with-slots ,(iter (for ((var-name slot-name) children) in specs)
                              (collecting (list var-name slot-name)))
               ,obj
             ,(1layer-slots specs))))))