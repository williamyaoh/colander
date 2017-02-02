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

(in-package #:colander)

(defun generate-parser-forms (dfa prods &optional (package *package*))
  (let ((*package* package)
        (noarg-code '(arg-spec des-spec opt-spec

                      identifier-char-p
                      short-opt-p long-opt-p
                      combined-short-opt-p single-opt-p
                      opt-p
                      included-arg-opt-p included-arg
                      expand-short maybe-expand fully-expand-args
                      normalize-args

                      parsing-function-declaims

                      parse-state
                      normalize-parse-state
                      parse-arg-state parse-des-state parse-opt-state
                      delegate-parse-state-transformer
                      parse-cli-spec

                      parse)))
    `(,@(iter (for symbol in noarg-code) (collecting (generate-code symbol)))
      ,@(generate-code 'prods prods)
      ,@(iter (for node in-vector (slot-value dfa 'nodes))
              (collecting (generate-code 'dfa-state-declaim node)))
      ,@(iter (for node in-vector (slot-value dfa 'nodes))
              (collecting (generate-code 'dfa-state node)))
      ,(generate-code 'arg-parse-driver dfa))))

(defun generate-copyable-parser (dfa prods)
  "Meant for generating a parser that will be copied into another file, like
   a standalone script.

   Certain read macros -- in particular, `#+` and `#-` -- don't have a
   corresponding Lisp representation. As such, if we want to generate
   copyable text for a standalone parser, we need to copy code that
   uses such read macros directly into our parser, as text."
  ;; We do the form generation in one package, and the printing in another.
  ;; We don't want FORMAT to print symbols while it itself is in the parser's
  ;; package.
  (unwind-protect
       (let ((forms (let ((*package* (make-package "COLANDER-PARSER" :use '(#:cl))))
                      `((defpackage #:colander-parser
                          (:use #:cl)
                          (:export #:parse))
                        ,(reintern-to-package `(defvar old-package))
                        ,(reintern-to-package `(eval-when (:compile-toplevel :load-toplevel :execute)
                                                 (setf old-package *package*)))
                        ,(reintern-to-package `(in-package #:colander-parser))
                        ;; We don't want to disturb any other code in the other file,
                        ;; so we surround our parser with package-defining code.
                        ,(squish-invisibles (slurp (relative-pathname "sexps/argv.sexp")))
                        ,@(generate-parser-forms dfa prods)
                        ,(reintern-to-package `(eval-when (:compile-toplevel :load-toplevel :execute)
                                                 (setf *package* old-package)))))))
         (with-output-to-string (out)
           (let ((*package* (make-package "COLANDER/BLANK" :use '())))
             (iter (for form in forms)
                   (if (stringp form)
                       (format out "~A~&" form)
                       (format out "~S~&" form))))))
    (when (find-package "COLANDER-PARSER")
      (delete-package "COLANDER-PARSER"))
    (when (find-package "COLANDER/BLANK")
      (delete-package "COLANDER/BLANK"))))


;;; Some temporary testing functions.

(defun cli-specs-to-parser% (&rest cli-specs)
  (let* ((prods (cli-specs-to-prods (map 'list #'normalize-spec% cli-specs)))
         (nfa (prods-to-nfa prods))
         (dfa (nfa-to-dfa nfa)))
    (list dfa prods)))

(defun output-parser-to-file% (parser filename)
  (destructuring-bind (dfa prods) parser
    (with-open-file (out filename
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (write-string (generate-copyable-parser dfa prods)
                    out))))
