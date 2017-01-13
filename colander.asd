(in-package #:asdf)
(defsystem #:colander
  :author "William Yao <williamyaoh@gmail.com>"
  :maintainer "William Yao <williamyaoh@gmail.com>"
  :version "0.0.1"
  :components ((:file "packages")
               (:file "colander" :depends-on ("packages"))))
