(defsystem #:colander
  :author "William Yao <williamyaoh@gmail.com>"
  :maintainer "William Yao <williamyaoh@gmail.com>"
  :version "0.0.1"
  :depends-on ("iterate" "alexandria")
  :components ((:file "packages")
               (:module "utils"
                :pathname ""
                :depends-on ("packages")
                :components ((:file "slots")
                             (:file "lists")
                             (:file "composition")
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
