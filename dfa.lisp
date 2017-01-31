(in-package #:colander/dfa)

(defclass dfa (finite-automaton) ())

(defclass dfa-node (node) ())

(defun epsilon-closure (nfa-node)
  (with-slots (edges) nfa-node
    (add-state nfa-node
               (epsilon-closures (iter (for transition in edges)
                                       (with-slots (label out) transition
                                         (when (null label) (collecting out))))))))

(defun epsilon-closures (nfa-nodes)
  (when (holdp nfa-nodes)
    (reduce
     (lambda (outs1 outs2)
       (union outs1 outs2 :test #'same-state-p))
     (map 'list #'epsilon-closure nfa-nodes)
     :initial-value '())))

(defun closure-outgoing-edges (closure)
  (alexandria:mappend
   (lambda (nfa-node)
     (iter (for transition in (slot-value nfa-node 'edges))
           (when (not (null (slot-value transition 'label)))
             (collecting (list nfa-node transition)))))
   closure))

(defun add-state (nfa-node nfa-nodes)
  (adjoin nfa-node nfa-nodes :test #'same-state-p))

(defmethod initialize-node ((fa-node dfa-node) nfa)
  (declare (ignorable nfa))
  (make-instance
   'dfa-node
   :datum (epsilon-closures (slot-value fa-node 'datum))))

(defmethod generate-root-node ((fa-type (eql 'dfa)) nfa)
  (make-instance
   'dfa-node
   :datum (list (slot-value nfa 'root))))

;; All slots, except ACCEPT-T, contain sets designating the set of NFA states,
;; which the DFA state which the transition leads to will represent.
;; ACCEPT-T represents the set of NFA states within the current DFA which have
;; accept transitions.
(defclass intermediate-dfa-transitions% ()
  ((arg-t :initarg :arg-t :initform '())
   (des-ts :initarg :des-ts :initform (make-hash-table :test 'equal))
   (opt-ts :initarg :opt-ts :initform (make-hash-table :test 'equal))
   (dd-t :initarg :dd-t :initform '())
   (accept-t :initarg :accept-t :initform '())))

(defmacro update (fn place)
  `(setf ,place (funcall ,fn ,place)))

(defun merge-transition-edge% (transitions transition-spec)
  (destructuring-bind (nfa-node transition) transition-spec
    (with-slots (label out) transition
      (if (null label)
          transitions
          (funcall (etypecase label
                     (arg-spec #'add-arg-t%)
                     (des-spec #'add-des-t%)
                     (opt-spec #'add-opt-t%)
                     (keyword
                      (ecase label
                        (:double-dash #'add-dd-t%)
                        (:accept #'add-accept-t%))))
                   transitions
                   nfa-node
                   label
                   out)))))

(defun add-arg-t% (transitions leaving-node arg-spec out)
  (declare (ignorable leaving-node arg-spec))
  (update (alexandria:curry #'add-state out)
          (slot-value transitions 'arg-t))
  transitions)
(defun add-des-t% (transitions leaving-node des-spec out)
  (declare (ignorable leaving-node))
  (update (alexandria:curry #'add-state out)
          (gethash (slot-value des-spec 'string)
                   (slot-value transitions 'des-ts)
                   '()))
  transitions)
(defun add-opt-t% (transitions leaving-node opt-spec out)
  (declare (ignorable leaving-node))
  (update (alexandria:curry #'add-state out)
          (gethash (slot-value opt-spec 'short) ; Will need to be updated.
                   (slot-value transitions 'opt-ts)
                   '()))
  transitions)
(defun add-dd-t% (transitions leaving-node double-dash out)
  (declare (ignore leaving-node double-dash))
  (update (alexandria:curry #'add-state out)
          (slot-value transitions 'dd-t))
  transitions)
(defun add-accept-t% (transitions leaving-node accept out)
  (declare (ignore accept out))
  (update (alexandria:curry #'add-state leaving-node)
          (slot-value transitions 'accept-t))
  transitions)

(defmethod generate-transitions ((node dfa-node) nfa)
  (with-slots (arg-t des-ts opt-ts dd-t accept-t)
      (reduce #'merge-transition-edge% (closure-outgoing-edges (slot-value node 'datum))
              :initial-value (make-instance 'intermediate-dfa-transitions%))
    `(,@(when (holdp accept-t) `((,accept-t :accept)))
      ,@(when (holdp dd-t) `((:double-dash ,(make-instance 'dfa-node :datum dd-t))))
      ,@(iter (for (des-string outs) in-hashtable des-ts)
              (collecting `(,(make-instance 'des-spec :string des-string)
                            ,(make-instance 'dfa-node :datum outs))))
      ,@(when (holdp arg-t) `((,(make-instance 'arg-spec :name nil)
                               ,(make-instance 'dfa-node :datum arg-t))))
      ,@(iter (for (opt-short outs) in-hashtable opt-ts)
              (collecting `(,(make-instance 'opt-spec :short opt-short :name nil)
                            ,(make-instance 'dfa-node :datum outs)))))))

(defmethod same-state-p ((node1 dfa-node) (node2 dfa-node))
  (let ((nfa-states1 (slot-value node1 'datum))
        (nfa-states2 (slot-value node2 'datum)))
    (and (subsetp nfa-states1 nfa-states2 :test #'same-state-p)
         (subsetp nfa-states2 nfa-states1 :test #'same-state-p))))

(defun nfa-to-dfa (nfa) (generate-finite-automaton 'dfa nfa))
