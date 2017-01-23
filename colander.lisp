(in-package #:colander)

;; Firstly, a standard syntax for our specifications.

(defclass arg-spec ()
  ((name :accessor arg-name :initarg :name)
   (many? :accessor arg-many? :initarg :many? :initform nil)))
(defclass des-spec ()
  ((string :accessor des-string :initarg :string)))
(defclass opt-spec ()
  ((name :accessor opt-name :initarg :name)
   (short :accessor opt-short :initarg :short)
   (arg? :accessor opt-arg? :initarg :arg? :initform nil)
   (arg-name :accessor opt-arg-name :initarg :arg-name :initform nil)
   (many? :accessor opt-many? :initarg :many? :initform nil)))

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


;; NFA, and NFA-to-DFA translations.

(defclass opt-arg-spec ()
  ((opt-spec :accessor contained-opt-spec :initarg :opt-spec)))

(defvar *nfa*)
(defvar *nfa-computed-transitions*)

(defclass transition ()
  ((edge :accessor transition-edge :initarg :edge)
   (out :accessor transition-out :initarg :out)))
(defclass nfa-node ()
  ((id :accessor nfa-node-id :initarg :id)
   (datum :accessor nfa-node-datum :initarg :datum)
   (transitions :accessor nfa-node-transitions :initarg :transitions :initform '())))
(defclass nfa ()
  ((root :accessor nfa-root :initarg :root)
   (lookup :accessor nfa-lookup :initarg :lookup
           :initform (make-array 5 :adjustable t :fill-pointer 0))))

(defclass nfa-accept-node (nfa-node) ())

(defgeneric same-state (obj1 obj2)
  (:documentation
   "Used to determine if a transition or node has been seen before
    (so we can cache it when necessary)")
  (:method (obj1 obj2) nil))

(defclass nfa-normal-parse-node (nfa-node) ())
(defclass nfa-parse-arg-node (nfa-node)
  ((previous :accessor parse-arg-previous :initarg :previous)))
(defclass nfa-arg-only-node (nfa-node) ())

(defmethod same-state ((obj1 opt-spec) (obj2 opt-spec))
  (eq obj1 obj2))

(defmethod same-state ((obj1 nfa-normal-parse-node) (obj2 nfa-normal-parse-node))
  (eq (nfa-node-datum obj1) (nfa-node-datum obj2)))
(defmethod same-state ((obj1 nfa-parse-arg-node) (obj2 nfa-parse-arg-node))
  (and (eq (parse-arg-previous obj1) (parse-arg-previous obj2))
       (same-state (nfa-node-datum obj1) (nfa-node-datum obj2))))
(defmethod same-state ((obj1 nfa-arg-only-node) (obj2 nfa-arg-only-node))
  (eq (nfa-node-datum obj1) (nfa-node-datum obj2)))

(defun lookup-state (state) (aref (nfa-lookup *nfa*) state))
(defun lookup-datum (state) (nfa-node-datum (lookup-state state)))

(defun intern-nfa-state (state)
  (let ((old-state (iter (for id index-of-vector (nfa-lookup *nfa*))
                         (finding id such-that (same-state state (lookup-state id))))))
    (if old-state
        old-state
        (setf (nfa-node-id state) (vector-push-extend state (nfa-lookup *nfa*))))))

;; (defun generate-new-nfa-state (type datum &rest keys)
;;   ;; EQ is sufficient for now. Need better checking in the future.
;;   (let ((old-state (iter (for state in (alexandria:hash-table-keys (nfa-lookup *nfa*)))
;;                          (finding state such-that (eq (lookup-datum state) datum)))))
;;     (if old-state
;;         old-state
;;         (let ((new-state (apply #'make-instance type :datum datum keys)))
;;           (vector-push-extend new-state (nfa-lookup *nfa*))))))

(defun spec-compute-specs-nfa (specs)
  "Determine an NFA from the specs."
  (let* ((*nfa* (make-instance 'nfa))
         (*nfa-computed-transitions* (make-hash-table))
         (root-node (make-instance 'nfa-node :datum nil)))
    (intern-nfa-state root-node)
    (setf (nfa-root *nfa*) root-node)
    (iter (for spec in specs)
          (push (make-instance 'transition
                               :edge nil
                               :out (intern-nfa-state
                                     (make-instance 'nfa-normal-parse-node
                                                    :datum spec)))
                (nfa-node-transitions root-node)))
    (iter (for transition in (nfa-node-transitions root-node))
          (spec-compute-transitions-nfa (transition-out transition)))
    *nfa*))

(defun spec-compute-transitions-nfa (state)
  (when (not (gethash state *nfa-computed-transitions*))
    (setf (gethash state *nfa-computed-transitions*) t)
    (let* ((node (aref (nfa-lookup *nfa*) state))
           (transitions (compute-transitions% node)))
      (iter (for (edge state) in transitions)
            (push (make-instance 'transition :edge edge :out state)
                  (nfa-node-transitions node)))
      (iter (for (edge state) in transitions)
            (spec-compute-transitions-nfa state)))))

(defgeneric compute-transitions% (node))

(defmethod compute-transitions% ((node nfa-normal-parse-node))
  (let* ((datum (nfa-node-datum node))
         (next (car datum)))
    `((double-dash
       ,(intern-nfa-state
         (make-instance 'nfa-arg-only-node
                        :datum (etypecase next
                                 (arg-spec datum)
                                 (des-spec datum)
                                 (null datum)
                                 (cons (cdr datum))))))
      ,@(etypecase next
          (arg-spec
           `((,next
              ,(intern-nfa-state
                (make-instance 'nfa-normal-parse-node :datum (cdr datum))))))
          (des-spec
           `((,next
              ,(intern-nfa-state
                (make-instance 'nfa-normal-parse-node :datum (cdr datum))))))
          (null `((accept ,(intern-nfa-state (make-instance 'nfa-accept-node)))))
          (cons
           (cons
            `(nil ,(intern-nfa-state
                    (make-instance 'nfa-normal-parse-node :datum (cdr datum))))
            (iter (for opt-spec in next)
                  (if (not (opt-arg? opt-spec))
                      (collecting `(,opt-spec ,(nfa-node-id node)))
                      (progn (collecting `(,(make-instance 'opt-arg-spec :opt-spec opt-spec)
                                            ,(nfa-node-id node)))
                             (collecting `(,opt-spec
                                           ,(intern-nfa-state
                                             (make-instance 'nfa-parse-arg-node
                                                            :datum opt-spec
                                                            :previous (nfa-node-id node))))))))))))))
(defmethod compute-transitions% ((node nfa-parse-arg-node))
  ;; We got here from an OPT-SPEC that has an arg, so the only outgoing edge
  ;; should be if we get an argument.
  `((,(make-instance 'arg-spec :name (opt-arg-name (nfa-node-datum node)))
     ,(parse-arg-previous node))))
(defmethod compute-transitions% ((node nfa-arg-only-node))
  (let* ((datum (nfa-node-datum node))
         (next (car datum)))
    (etypecase next
      (arg-spec `((,next ,(intern-nfa-state
                           (make-instance 'nfa-arg-only-node
                                          :datum (drop-if #'listp (cdr datum)))))))
      (des-spec `((,next ,(intern-nfa-state
                           (make-instance 'nfa-arg-only-node
                                          :datum (drop-if #'listp (cdr datum)))))))
      (null `((accept ,(intern-nfa-state (make-instance 'nfa-accept-node))))))))
(defmethod compute-transitions% ((node nfa-accept-node)) '())


;; DFA.

(defun epsilon-closure (state)
  (cons state (iter (for transition in (nfa-node-transitions state))
                    (when (null (transition-edge transition))
                      (appending
                       (epsilon-closure (lookup-state (transition-out transition))))))))

(defun epsilon-closures (states)
  (iter (for state in states) (unioning (epsilon-closure state))))

(defclass dfa-node ()
  ((arg-ts :accessor dfa-node-arg-ts :initarg :arg-ts :initform nil)
   (des-ts :accessor dfa-node-des-ts :initarg :des-ts :initform nil)
   (dd-t :accessor dfa-node-dd-t :initarg :dd-t :initform nil)
   (accept-t :accessor dfa-node-accept-t :initarg :accept-t :initform nil)
   (opt-ts :accessor dfa-node-opt-ts :initarg :opt-ts :initform nil)
   (opt-arg-ts :accessor dfa-node-opt-arg-ts :initarg :opt-arg-ts :initform nil)))
