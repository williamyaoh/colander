(in-package #:cl-user)

(defpackage #:colander/utils
  (:use #:cl
        #:iterate)
  (:export #:with-nested-slots

           #:holdp
           #:singlep

           #:take #:take-if
           #:drop #:drop-if

           #:segregate-binary
           #:segregate

           #:suffixes

           #:flatten
           #:group

           #:slurp #:spit

           #:eval-now))

(defpackage #:colander/code-generation
  (:use #:cl
        #:colander/utils)
  (:export #:generate-code
           #:symb
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

           #:arg-spec-load-form
           #:des-spec-load-form
           #:opt-spec-load-form

           ;; For testing purposes.
           #:normalize-spec%))

(defpackage #:colander/finite-automata
  (:use #:cl
        #:iterate

        #:colander/similar-p)
  (:export #:node #:id #:datum #:edges
           #:edge #:label #:out
           #:finite-automaton #:root #:nodes

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

           #:nfa #:root #:nodes
           #:nfa-node #:id #:datum #:edges

           #:label #:out

           #:nfa-start-node
           #:nfa-normal-node
           #:nfa-opt-arg-parse-node #:opt-spec
           #:nfa-dd-node))

(defpackage #:colander/dfa
  (:use #:cl
        #:iterate

        #:colander/utils
        #:colander/finite-automata
        #:colander/similar-p
        #:colander/op-specs
        #:colander/nfa)
  (:export #:nfa-to-dfa

           #:dfa #:root #:nodes
           #:dfa-node #:id #:datum #:edges

           #:label #:out))

(defpackage #:colander/parser
  (:use #:cl
        #:iterate

        #:colander/utils
        #:colander/op-specs
        #:colander/finite-automata
        #:colander/code-generation
        #:colander/dfa)
  (:export #:identifier-char-p
           #:short-opt-p
           #:long-opt-p
           #:combined-short-opt-p
           #:single-opt-p
           #:opt-p
           #:included-arg-opt-p
           #:included-arg
           #:expand-short
           #:maybe-expand
           #:fully-expand-args
           #:normalize-args

           #:prods

           #:arg-parse-driver
           #:dfa-state-declaim
           #:dfa-state

           #:parse-state
           #:parse-cli-spec
           #:parsing-function-declaims
           #:normalize-parse-state
           #:delegate-parse-state-transformer
           #:parse-arg-state
           #:parse-des-state
           #:parse-opt-state

           #:parse))

(defpackage #:colander
  (:use #:cl
        #:iterate

        #:colander/utils
        #:colander/code-generation
        #:colander/op-specs
        #:colander/finite-automata
        #:colander/nfa
        #:colander/dfa
        #:colander/parser))
