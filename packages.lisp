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

           #:slurp #:spit))

(defpackage #:colander/defschema
  (:use #:cl
        #:colander/utils)
  (:export #:defschema))

(defpackage #:colander
  (:use #:cl
        #:colander/utils
        #:colander/defschema
        #:iterate))
