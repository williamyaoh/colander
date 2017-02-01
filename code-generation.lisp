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

(in-package #:colander/code-generation)

(eval-now
  (defgeneric generate-code (code-name &rest args)
    (:documentation
     "Provide a way of generating code _into a different package_.
      Core of our parser generator; allows us to emit code without dependencies
      to parse command line options.")))

(defmacro generate-and-load (code-name &rest args)
  (apply #'generate-code code-name args))

(defun symb (&rest objs)
  (intern (string-upcase (with-output-to-string (s) (format s "~{~A~}" objs)))))

;; We use REINTERN-TO-PACKAGE within the definition of DEFCODE below,
;; but we also need it in our output code.
(eval-now
  (defmethod generate-code ((code-name (eql 'reintern-to-package)) &rest args)
    (declare (ignorable args))
    `(defun reintern-to-package (form &optional (package *package*))
       (typecase form
         (cons (map 'list (lambda (form) (reintern-to-package form package)) form))
         (symbol (if (or (keywordp form) (not (symbol-package form)))
                     form
                     (intern (symbol-name form) package)))
         (otherwise form))))

  (generate-and-load reintern-to-package)

  (defmethod generate-code :around ((code-name (eql 'reintern-to-package)) &rest args)
    (declare (ignorable args))
    (reintern-to-package (call-next-method))))

(defmacro defcode (name (&rest code-fn-args) &body body)
  "BDDY should return some Lisp code; DEFCODE provides a generic function to
   automatically reintern said code in a different package."
  (let ((code-name (gensym "CODE-NAME"))
        (inner-args (gensym "ARGS")))
    `(defmethod generate-code ((,code-name (eql ',name)) &rest ,inner-args)
       (destructuring-bind ,code-fn-args ,inner-args
         (reintern-to-package
          (progn ,@body))))))

(defmacro defcode! (name &body body)
  "Used for defining code that's useful both in the current package
   and for generating somewhere else. BODY must be evaluable at compile time."
  `(eval-now
     (defcode ,name () ,@body)
     (generate-and-load ,name)))

(defmacro defcodefn! (name (&rest args) &body body)
  "Used for defining functions which are useful both in the current package
   and for generating somewhere else."
  `(defcode! ,name '(defun ,name ,args ,@body)))
