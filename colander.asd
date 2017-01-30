(defsystem #:colander
  :author "William Yao <williamyaoh@gmail.com>"
  :maintainer "William Yao <williamyaoh@gmail.com>"
  :version "0.0.1"
  :depends-on ("iterate" "alexandria")
  :components ((:file "packages")
               (:file "lists" :depends-on ("packages"))
               (:file "composition" :depends-on ("packages"))
               (:module "utils"
                :pathname ""
                :depends-on ("packages")
                :components ((:file "slots")
                             (:file "lists")
                             (:file "composition")
                             (:file "files")))
               (:module "sexps"
                :components ((:static-file "argv.sexp")))
               (:file "defschema" :depends-on ("packages" "utils"))
               (:file "colander" :depends-on ("packages"
                                              "utils"
                                              "defschema"
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
