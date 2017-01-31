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
