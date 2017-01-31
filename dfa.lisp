(in-package #:colander/dfa)

(defclass dfa (finite-automaton) ())

(defclass dfa-node (node) ())

(defun epsilon-closure (nfa-node)
  (with-slots (edges) nfa-node
    (adjoin
     nfa-node
     (epsilon-closures (iter (for transition in edges)
                             (with-slots (label out) transition
                               (when (null label) (collecting out)))))
     :test #'same-state-p)))

(defun epsilon-closures (nfa-nodes)
  (when (holdp nfa-nodes)
    (reduce
     (lambda (outs1 outs2)
       (union outs1 outs2 :test #'same-state-p))
     (map 'list #'epsilon-closure nfa-nodes)
     :initial-value '())))

(defmethod initialize-node ((fa-node dfa-node) nfa)
  (declare (ignorable nfa))
  (make-instance
   'dfa-node
   :datum (epsilon-closures (slot-value fa-node 'datum))))

(defmethod generate-root-node ((fa-type (eql 'dfa)) nfa)
  (make-instance
   'dfa-node
   :datum (list (slot-value nfa 'root))))

(defclass intermediate-dfa-transitions% ()
  ((arg-t :initarg :arg-t :initform '())
   (des-ts :initarg :des-ts :initform (make-hash-table :test 'equal))
   (opt-ts :initarg :opt-ts :initform (make-hash-table :test 'equal))
   (dd-t :initarg :dd-t :initform '())
   (accept-t :initarg :accept-t :initform '())))

(defmacro update (fn place)
  `(setf ,place (funcall ,fn ,place)))

(defun merge-transition-edge% (transitions transition)
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
                 label
                 out))))

(defun add-arg-t% (transitions arg-spec out)
  (declare (ignorable arg-spec))
  (update (lambda (nfa-nodes) (adjoin out nfa-nodes :test #'same-state-p))
          (slot-value transitions 'arg-t))
  transitions)
(defun add-des-t% (transitions des-spec out)
  ;; TODO
  )
(defun add-opt-t% (transitions opt-spec out)
  ;; TODO
  )
(defun add-dd-t% (transitions double-dash out)
  (declare (ignore double-dash))
  (update (lambda (nfa-nodes) (adjoin out nfa-nodes :test #'same-state-p))
          (slot-value transitions 'dd-t))
  transitions)
(defun add-accept-t% (transitions accept out)
  (declare (ignore accept))
  (update (lambda (nfa-nodes) (adjoin out nfa-nodes :test #'same-state-p))
          (slot-value transitions 'accept-t))
  transitions)

(defmethod generate-transitions ((node dfa-node) nfa)
  ;; TODO: Probably gonna be pretty hard to pull this off.
  (with-slots (arg-t des-ts opt-ts dd-t accept-t)
      (reduce #'merge-transition-edge% (slot-value node 'edges)
              :initial-value (make-instance 'intermediate-dfa-transitions%))
    `(
      ;; stuff here?
      ))
  )

(defmethod same-state-p ((node1 dfa-node) (node2 dfa-node))
  (let ((nfa-states1 (slot-value node1 'datum))
        (nfa-states2 (slot-value node2 'datum)))
    (and (subsetp nfa-states1 nfa-states2 :test #'same-state-p)
         (subsetp nfa-states2 nfa-states1 :test #'same-state-p))))
