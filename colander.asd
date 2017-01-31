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
               (:module "sexps"
                :components ((:static-file "argv.sexp")))
               (:file "code-generation" :depends-on ("packages" "utils"))
               (:file "finite-automata" :depends-on ("packages"))
               (:file "colander" :depends-on ("packages"
                                              "utils"
                                              "code-generation"
                                              "finite-automata"
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
