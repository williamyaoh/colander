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
           #:relative-pathname

           #:lines #:unlines
           #:trim-comment #:trim-trailing-whitespace
           #:squish-invisibles

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
        #:colander/op-specs
        #:colander/code-generation
        #:colander/nfa
        #:colander/dfa
        #:colander/parser)
  (:export #:cli-specs-to-parser%
           #:output-parser-to-file%))
