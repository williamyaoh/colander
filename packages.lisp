(in-package #:cl-user)

(defpackage #:colander/utils
  (:use #:cl)
  (:export #:take #:take-if
           #:drop #:drop-if

           #:segregate-binary
           #:segregate

           #:mappend

           #:suffixes

           #:one-of #:all-of))

(defpackage #:colander/defschema
  (:use #:cl
        #:colander/utils)
  (:export #:defschema))

(defpackage #:colander
  (:use #:cl
        #:colander/utils
        #:colander/defschema))
