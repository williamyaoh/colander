(in-package #:colander)

(defgeneric similar-p (obj1 obj2)
  (:documentation
   "I don't know why EQUAL isn't a generic function.")
  (:method (obj1 obj2)
    (declare (ignore obj1 obj2))
    nil))

;; Firstly, a standard syntax for our specifications.
;; This syntax needs to be readable, because we need to be able
;; to copy it into our generated parser...

(defcode! arg-spec
  `(defclass arg-spec ()
     ((name :accessor arg-name :initarg :name)
      (many? :accessor arg-many? :initarg :many? :initform nil))))
(defcode! des-spec
  `(defclass des-spec ()
     ((string :accessor des-string :initarg :string))))
(defcode! opt-spec
  `(defclass opt-spec ()
     ((name :accessor opt-name :initarg :name)
      (short :accessor opt-short :initarg :short)
      (arg? :accessor opt-arg? :initarg :arg? :initform nil)
      (arg-name :accessor opt-arg-name :initarg :arg-name :initform nil)
      (many? :accessor opt-many? :initarg :many? :initform nil))))

(defmethod similar-p ((obj1 arg-spec) (obj2 arg-spec))
  (eq (arg-name obj1) (arg-name obj2)))

(defmethod similar-p ((obj1 des-spec) (obj2 des-spec))
  (string= (des-string obj1) (des-string obj2)))

(defmethod similar-p ((obj1 opt-spec) (obj2 opt-spec))
  ;; This will need to get rewritten after adding more functionality to
  ;; our options.
  (and (string= (opt-short obj1) (opt-short obj2))
       (not (alexandria:xor (opt-arg? obj1) (opt-arg? obj2)))))

(defcode! arg-spec-load-form
  `(defmethod make-load-form ((obj arg-spec) &optional environment)
     (declare (ignorable environment))
     ;; Due to backquote not having a consistent internal representation
     ;; across implementations, this template has to be written manually...
     (reintern-to-package
      (list 'make-instance (list 'quote (type-of obj))
            :name (arg-name obj)
            :many? (arg-many? obj)))))
(defcode! des-spec-load-form
  `(defmethod make-load-form ((obj des-spec) &optional environment)
     (declare (ignorable environment))
     (reintern-to-package
      (list 'make-instance (list 'quote (type-of obj))
            :string (des-string obj)))))
(defcode! opt-spec-load-form
  `(defmethod make-load-form ((obj opt-spec) &optional environment)
     (declare (ignorable environment))
     (reintern-to-package
      (list 'make-instance (list 'quote (type-of obj))
            :name (opt-name obj)
            :short (opt-short obj)
            :arg? (opt-arg? obj)
            :arg-name (opt-arg-name obj)
            :many? (opt-many? obj)))))

(defmethod print-object ((obj arg-spec) stream)
  (format stream "~S" (make-load-form obj)))
(defmethod print-object ((obj des-spec) stream)
  (format stream "~S" (make-load-form obj)))
(defmethod print-object ((obj opt-spec) stream)
  (format stream "~S" (make-load-form obj)))

(defun arg-spec-list-p% (obj) (symbolp obj))
(defun des-spec-list-p% (obj) (stringp obj))
(defun opt-spec-list-p% (obj)
  (and (consp obj)
       (member (car obj) '(:opt :opts :arg))))

(defun mk-arg-spec% (obj)
  (make-instance 'arg-spec :name obj))
(defun mk-des-spec% (obj)
  (make-instance 'des-spec :string obj))
(defun mk-opt-spec% (obj)
  (ecase (car obj)
    (:opt (make-instance 'opt-spec :short (cadr obj)))
    (:arg (make-instance 'opt-spec :short (cadr obj) :arg? t :arg-name (caddr obj)))))

(defun explode-opt-specs% (specs)
  (iter outer
        (for spec in specs)
        (ecase (car spec)
          ((:opt :arg) (collecting (mk-opt-spec% spec)))
          (:opts (iter (for char in-vector (subseq (cadr spec) 1))
                       (in outer (collecting (mk-opt-spec% `(:opt ,(format nil "-~C" char))))))))))

(defun normalize-spec% (spec)
  (iter (while (holdp spec))
        (let ((next (car spec)))
          (cond
            ((arg-spec-list-p% next)
             (collecting (mk-arg-spec% next))
             (setf spec (cdr spec)))
            ((des-spec-list-p% next)
             (collecting (mk-des-spec% next))
             (setf spec (cdr spec)))
            ((opt-spec-list-p% next)
             (multiple-value-bind (taken left) (take-if #'opt-spec-list-p% spec)
               (collecting (explode-opt-specs% taken))
               (setf spec left)))))))



(defclass production ()
  ((cli-spec :initarg :cli-spec)))
(defclass item ()
  ((prod-id :initarg :prod-id)
   (dot :initarg :dot)))

(defmethod similar-p ((obj1 item) (obj2 item))
  (and (= (slot-value obj1 'prod-id) (slot-value obj2 'prod-id))
       (= (slot-value obj1 'dot) (slot-value obj2 'dot))))

(defun item-advance (item)
  (with-slots (dot prod-id) item
    (make-instance 'item :prod-id prod-id :dot (1+ dot))))
(defun item-prod (item prods)
  (aref prods (slot-value item 'prod-id)))
(defun item-at-dot (item prods)
  (nth (slot-value item 'dot) (item-prod item prods)))



(defclass nfa (finite-automaton) ())

(defclass nfa-node (node) ())
(defclass nfa-start-node (nfa-node) ())
(defclass nfa-normal-node (nfa-node) ())
(defclass nfa-opt-arg-parse-node (nfa-node)
  ((opt-spec :initarg :opt-spec)))
(defclass nfa-dd-node (nfa-node) ())

(defun next-cli-spec (nfa-node prods)
  (with-slots ((item datum)) nfa-node
    (item-at-dot item prods)))
(defun node-advance (nfa-node)
  (with-slots ((item datum)) nfa-node
    (setf item (item-advance item))))

(defmethod generate-root-node ((fa-type (eql 'nfa)) prods)
  (make-instance 'nfa-start-node :datum prods))

(defmethod generate-transitions :around (node seed)
  (declare (ignore seed))
  (format t "Generating transitions for: ~A~%" (slot-value node 'id))
  (format t "Type: ~A~%" (type-of node))
  (call-next-method))

(defmethod generate-transitions ((node nfa-start-node) prods)
  (declare (ignore prods))
  (with-slots ((prods datum)) node
    (iter (for i index-of-vector prods)
          (collecting `(nil ,(make-instance
                              'nfa-normal-node
                              :datum (make-instance
                                      'item
                                      :prod-id i
                                      :dot 0)))))))

(defmethod generate-transitions ((node nfa-normal-node) prods)
  (with-slots ((item datum)) node
    `((double-dash ,(make-instance 'nfa-dd-node :datum item))
      ,@(let ((next (next-cli-spec node prods)))
          (etypecase next
            (arg-spec `((,next ,(make-instance 'nfa-normal-node :datum (item-advance item)))))
            (des-spec `((,next ,(make-instance 'nfa-normal-node :datum (item-advance item)))))
            (null `((accept accept)))
            (cons `((nil ,(make-instance 'nfa-normal-node :datum (item-advance item)))
                    ,@(iter (for opt-spec in next)
                            (if (not (opt-arg? opt-spec))
                                (collecting `(,opt-spec ,node))
                                (collecting `(,opt-spec ,(make-instance
                                                          'nfa-opt-arg-parse-node
                                                          :opt-spec opt-spec
                                                          :datum item))))))))))))

(defmethod generate-transitions ((node nfa-opt-arg-parse-node) prods)
  (declare (ignore prods))
  (with-slots ((previous datum) opt-spec) node
    (list `(,(make-instance 'arg-spec :name (opt-arg-name opt-spec))
            ,(make-instance 'nfa-normal-node :datum previous)))))

(defmethod generate-transitions ((node nfa-dd-node) prods)
  (let ((next (next-cli-spec node prods)))
    (list (with-slots ((item datum)) node
            (etypecase next
              (arg-spec `(,next ,(make-instance 'nfa-dd-node :datum (item-advance item))))
              (des-spec `(,next ,(make-instance 'nfa-dd-node :datum (item-advance item))))
              (null `(accept accept)))))))

(defmethod same-state-p ((node1 nfa-normal-node) (node2 nfa-normal-node))
  (similar-p (slot-value node1 'datum) (slot-value node2 'datum)))
(defmethod same-state-p ((node1 nfa-opt-arg-parse-node) (node2 nfa-opt-arg-parse-node))
  (and (similar-p (slot-value node1 'opt-spec) (slot-value node2 'opt-spec))
       (similar-p (slot-value node1 'datum) (slot-value node2 'datum))))
(defmethod same-state-p ((node1 nfa-dd-node) (node2 nfa-dd-node))
  (similar-p (slot-value node1 'datum) (slot-value node2 'datum)))

(defmethod initialize-node ((fa-node nfa-dd-node) prods)
  (with-slots ((item datum)) fa-node
    (let ((next (make-instance 'nfa-dd-node :datum item)))
      (with-slots ((item datum)) next
        (iter (while (consp (next-cli-spec next prods)))
              (setf item (item-advance item)))
        next))))

(defun cli-specs-to-nfa (cli-specs)
  ;; CLI-SPECS :: (ARRAY * CLI-SPEC)
  (generate-finite-automaton 'nfa cli-specs))
