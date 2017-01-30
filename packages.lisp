(in-package #:cl-user)

(defpackage #:colander/utils
  (:use #:cl
        #:iterate)
  (:export #:with-nested-slots

           #:holdp

           #:take #:take-if
           #:drop #:drop-if

           #:segregate-binary
           #:segregate

           #:mappend

           #:suffixes

           #:flatten
           #:group

           #:one-of #:all-of

           #:partial

           #:slurp #:spit))

(defpackage #:colander/finite-automata
  (:use #:cl
        #:iterate)
  (:export #:node #:id #:datum #:edges
           #:edge #:label #:out
           #:finite-automaton #:root #:nodes

           #:generate-finite-automaton
           #:generate-root-node
           #:effect-transitions-recursively
           #:effect-transitions
           #:generate-transitions
           #:intern-node
           #:same-state-p))

(defpackage #:colander
  (:use #:cl
        #:colander/utils
        #:colander/finite-automata
        #:iterate))
