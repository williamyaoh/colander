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

(defpackage #:colander/similar-p
  (:use #:cl)
  (:export #:similar-p))

(defpackage #:colander/op-specs
  (:use #:cl
        #:iterate

        #:colander/utils
        #:colander/code-generation
        #:colander/similar-p)
  (:export #:arg-spec #:name #:arg-name #:many? #:arg-many?
           #:des-spec #:string #:des-string
           #:opt-spec
           #:name #:opt-name
           #:short #:opt-short
           #:arg? #:opt-arg?
           #:arg-name #:opt-arg-name
           #:many? #:opt-many?

           ;; For testing purposes.
           #:normalize-spec%))

(defpackage #:colander/finite-automata
  (:use #:cl
        #:iterate

        #:colander/similar-p)
  (:export #:node #:id #:datum #:edges
           #:edge #:label #:out
           #:finite-automaton #:root #:nodes

           #:accept

           #:generate-finite-automaton
           #:generate-root-node
           #:generate-transitions
           #:initialize-node
           #:same-state-p

           #:production #:cli-spec
           #:item #:prod-id #:dot

           #:item-advance
           #:item-prod
           #:item-at-dot))

(defpackage #:colander/nfa
  (:use #:cl
        #:iterate

        #:colander/finite-automata
        #:colander/similar-p
        #:colander/op-specs)
  (:export #:cli-specs-to-prods
           #:prods-to-nfa

           #:nfa
           #:nfa-node
           #:nfa-start-node
           #:nfa-normal-node
           #:nfa-opt-arg-parse-node #:opt-spec
           #:nfa-dd-node))

(defpackage #:colander
  (:use #:cl
        #:iterate

        #:colander/utils
        #:colander/code-generation
        #:colander/op-specs
        #:colander/finite-automata
        #:colander/nfa))
