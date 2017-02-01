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

(in-package #:colander/utils)

(defun normalize-slot-designator (slot-des)
  (etypecase slot-des
    (symbol (list slot-des slot-des))
    (cons slot-des)))

(defun normalize-slot-spec (slot-spec)
  (cond
    ((or (symbolp slot-spec)
         (and (consp slot-spec) (symbolp (car slot-spec))
              (cadr slot-spec) (symbolp (cadr slot-spec))))
     (list (normalize-slot-designator slot-spec) '()))
    ((and (consp slot-spec) (cdr slot-spec) (listp (cadr slot-spec)))
     (list (normalize-slot-designator (car slot-spec))
           (map 'list #'normalize-slot-spec (cadr slot-spec))))
    (:otherwise
     (error "Malformed slot specification: ~S" slot-spec))))

(defmacro with-nested-slots ((&rest slot-specs) obj &body body)
  "Like WITH-SLOTS, but allows for binding the slots of nested objects.

   SLOT-SPECS ::= (SPEC [SPEC...])

   SPEC
     ::= SLOT-DESIGNATOR
       | (SLOT-DESIGNATOR SLOT-SPECS)

   SLOT-DESIGNATOR
     ::= SLOT-NAME
       | (VAR-NAME SLOT-NAME)

   SLOT-NAME, VAR-NAME ::= SYMBOL"
  (let ((specs (map 'list #'normalize-slot-spec slot-specs)))
    ;; WITH-NESTED-SLOTS% expands recursively, so we do sanity-checking
    ;; on the slot specifications once, at the top-level.
    (labels ((var-names (specs)
               (when specs
                 (iter (for ((var-name slot-name) children) in specs)
                       (collecting var-name)
                       (appending (var-names children))))))
      (when (not (equal (var-names specs) (remove-duplicates (var-names specs))))
        (error "A variable name occurs more than once in WITH-NESTED-SLOTS."))
      `(with-nested-slots% ,specs ,obj ,@body))))

(defmacro with-nested-slots% ((&rest slot-specs) obj &body body)
  (if (null slot-specs)
      `(progn ,obj ,@body)
      (labels ((1layer-slots (specs)
                 (if (null specs)
                     `(progn ,@body)
                     (destructuring-bind ((var-name slot-name) children) (first specs)
                       (declare (ignorable slot-name))
                       `(with-nested-slots% ,children ,var-name
                          ,(1layer-slots (rest specs)))))))
        `(with-slots ,(iter (for ((var-name slot-name) children) in slot-specs)
                            (collecting (list var-name slot-name)))
             ,obj
           ,(1layer-slots slot-specs)))))
