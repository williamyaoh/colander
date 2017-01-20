(in-package #:asdf)
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
                  :components ((:file "lists")
                               (:file "composition")))
               (:file "defschema" :depends-on ("packages" "utils"))
               (:file "colander" :depends-on ("packages" "utils" "defschema"))))
