;;;; Copyright (c) 2017, William Yao
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are
;;;; met:
;;;;
;;;;  * Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;;  * Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the
;;;;    distribution.
;;;;  * Neither the name of William Yao nor the names of other contributors
;;;;    may be used to endorse or promote products derived from this
;;;;    software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PUROPSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
;;;   - SAME-STATE-P
;;; Additionally, methods on INITIALIZE-NODE can be used to do
;;; initialization on newly-created nodes.

;;; NIL as an edge label represents an epsilon transition.

(defgeneric generate-finite-automaton (fa-type seed)
  (:documentation
   "Return a finite automata of type FA-TYPE. SEED should be some sort of
    designator for the start state, which is used to generate the rest of
    the finite automata.")
  (:method (fa-type seed)
    (let* ((fa (make-instance fa-type))
           (root-node (intern-node (generate-root-node fa-type seed) fa seed))
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
  (:method :around (fa-node fa seed computed)
    (with-slots (id) fa-node
      (cond ((gethash id computed) nil)
            (:otherwise
             (setf (gethash id computed) t)
             (call-next-method)))))
  (:method (fa-node fa seed computed)
    (effect-transitions fa-node fa seed)
    (with-slots (edges) fa-node
      (iter (for edge in edges)
            (with-slots (out) edge
              (unless (eq out :accept)
                (effect-transitions-recursively out fa seed computed)))))))

(defgeneric effect-transitions (fa-node fa seed)
  (:documentation
   "Add just the transitions at this node, to that node.")
  (:method (fa-node fa seed)
    (with-slots (edges) fa-node
      (iter (for (label out) in (generate-transitions fa-node seed))
            (push (make-instance
                   'edge
                   :label label
                   :out (if (eq out :accept)
                            :accept
                            (intern-node out fa seed)))
                  edges)))))

(defgeneric generate-transitions (fa-node seed)
  (:documentation
   "Required for a finite automata to implement. Return a list of form
    ((LABEL OUT)...), where LABEL is a datum to put into the edge transition,
    and OUT is a node object representing the state that this transition leads
    to."))

(defgeneric initialize-node (fa-node seed)
  (:documentation
   "Can be defined by a finite automata implementation. Performs initialization
    on a node before passing it to INTERN-NODE to be added to the finite
    automaton.")
  (:method (fa-node seed)
    (declare (ignore seed))
    fa-node))

(defgeneric intern-node (fa-node fa seed)
  (:documentation
   "Can be wrapped by a finite automata implementation. Checks to see if a node
    representing the same state already exists in the given finite automata,
    and returns it if so; otherwise adds FA-NODE to FA.

    Given that there's no guarantee that a finite automata is acyclic, a correct
    implementation of SAME-STATE-P is crucial to avoid infinite loops.")
  (:method (fa-node fa seed)
    (with-slots (nodes) fa
      (let* ((fa-node (initialize-node fa-node seed))
             (existing (iter (for fa-node* in-vector nodes)
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
