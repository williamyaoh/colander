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

(defsystem #:colander
  :author "William Yao <williamyaoh@gmail.com>"
  :maintainer "William Yao <williamyaoh@gmail.com>"
  :version "0.0.1"
  :depends-on ("iterate" "alexandria")
  :components ((:file "packages")
               (:module "utils"
                :depends-on ("packages")
                :components ((:file "slots")
                             (:file "lists")
                             (:file "files")
                             (:file "evaluation")))
               (:file "code-generation" :depends-on ("packages" "utils"))
               (:file "similar-p" :depends-on ("packages"))
               (:file "op-specs" :depends-on ("packages"
                                              "utils"
                                              "similar-p"))
               (:module "sexps"
                :components ((:static-file "argv.sexp")))
               (:module "automata"
                :pathname ""
                :depends-on ("packages" "similar-p")
                :components ((:file "finite-automata")
                             (:file "productions")))
               (:file "nfa" :depends-on ("packages"
                                         "automata"
                                         "similar-p"
                                         "op-specs"))
               (:file "dfa" :depends-on ("packages"
                                         "utils"
                                         "automata"
                                         "similar-p"
                                         "op-specs"
                                         "nfa"))
               (:file "colander" :depends-on ("packages"
                                              "utils"
                                              "code-generation"
                                              "op-specs"
                                              "automata"
                                              "nfa"
                                              "sexps")))
  :in-order-to ((test-op (test-op "colander/test"))))

(defsystem #:colander/test
  :author "William Yao <williamyaoh@gmail.com>"
  :maintainer "William Yao <williamyaoh@gmail.com>"
  :version "0.0.1"
  :depends-on ("colander" "fiveam")
  :components ()
  :perform (test-op (o s)
                    nil))
