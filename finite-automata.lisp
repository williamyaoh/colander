(in-package #:colander/finite-automata)

(defclass node ()
  ((id :initarg :id)
   (datum :initarg :datum)
   (edges :initarg :edges
          :initform '())))
(defclass edge ()
  ((label :initarg :label)
   (out :initarg :out)))
(defclass finite-automaton ()
  ((root :initarg :root)
   (nodes :initarg :nodes
          :initform (make-array 5 :adjustable t :fill-pointer 0))))

;;; A DFA or NFA only needs to implement the following functions to work
;;; properly:
;;;   - GENERATE-ROOT-NODE
;;;   - GENERATE-TRANSITIONS
;;;   - SAME-STATE
;;; Additionally, :AROUND methods on INTERN-NODE can be used to do
;;; initialization on newly-created nodes.

;;; NIL as an edge label represents an epsilon transition.

(defgeneric generate-finite-automaton (fa-type seed)
  (:documentation
   "Return a finite automata of type FA-TYPE. SEED should be some sort of
    designator for the start state, which is used to generate the rest of
    the finite automata.")
  (:method (fa-type seed)
    (let* ((fa (make-instance fa-type))
           (root-node (intern-node (generate-root-node fa-type seed) fa))
           (computed (make-hash-table :test 'eql)))
      (with-slots (root) fa
        (setf root root-node)
        (effect-transitions-recursively root-node fa seed computed)
        fa))))

(defgeneric generate-root-node (fa-type seed)
  (:documentation
   "Required for a finite automata to implement. Return node object corresponding
    to the root of the finite automata; does not need transitions."))

(defgeneric effect-transitions-recursively (fa-node fa seed computed)
  (:documentation
   "Recursively create all the states and transitions reachable from FA-NODE.")
  (:method (fa-node fa seed computed)
    (effect-transitions fa-node fa seed computed)
    (with-slots (edges) fa-node
      (iter (for edge in edges)
            (with-slots (out) edge
              (effect-transitions-recursively out fa seed computed))))))

(defgeneric effect-transitions (fa-node fa seed computed)
  (:documentation
   "Add just the transitions at this node, to that node.")
  (:method :around (fa-node fa seed computed)
    (with-slots (id) fa-node
      (cond ((gethash id computed) nil)
            (:otherwise
             (setf (gethash id computed) t)
             (call-next-method)))))
  (:method (fa-node fa seed computed)
    (with-slots (edges) fa-node
      (iter (for (label out) in (generate-transitions fa-node seed))
            (push (make-instance
                   'edge
                   :label label
                   :out (if (eq out 'accept)
                            'accept
                            (intern-node out fa)))
                  edges)))))

(defgeneric generate-transitions (fa-node seed)
  (:documentation
   "Required for a finite automata to implement. Return a list of form
    ((LABEL OUT)...), where LABEL is a datum to put into the edge transition,
    and OUT is a node object representing the state that this transition leads
    to."))

(defgeneric intern-node (fa-node fa)
  (:documentation
   "Can be wrapped by a finite automata implementation. Checks to see if a node
    representing the same state already exists in the given finite automata,
    and returns it if so; otherwise adds FA-NODE to FA.

    Given that there's no guarantee that a finite automata is acyclic, a correct
    implementation of SAME-STATE-P is crucial to avoid infinite loops.")
  (:method (fa-node fa)
    (with-slots (nodes) fa
      (let ((existing (iter (for fa-node* in-vector nodes)
                            (finding fa-node* such-that (same-state-p fa-node fa-node*)))))
        (if existing
            existing
            (with-slots (id) fa-node
              (setf id (vector-push-extend fa-node nodes))
              fa-node))))))

(defgeneric same-state-p (fa-node1 fa-node2)
  (:documentation
   "Requires for a finite automata to implement.
    Test if the nodes represent the same state.")
  (:method (fa-node1 fa-node2)
    (declare (ignore fa-node1 fa-node2))
    nil))
