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

           #:slurp #:spit

           #:eval-now))

(defpackage #:colander/code-generation
  (:use #:cl
        #:colander/utils)
  (:export #:generate-code
           #:reintern-to-package

           #:defcode
           #:defcode!
           #:defcodefn!))

(defpackage #:colander/finite-automata
  (:use #:cl
        #:iterate)
  (:export #:node #:id #:datum #:edges
           #:edge #:label #:out
           #:finite-automaton #:root #:nodes

           #:accept

           #:generate-finite-automaton
           #:generate-root-node
           #:generate-transitions
           #:initialize-node
           #:same-state-p))

(defpackage #:colander
  (:use #:cl
        #:colander/utils
        #:colander/code-generation
        #:colander/finite-automata
        #:iterate))
