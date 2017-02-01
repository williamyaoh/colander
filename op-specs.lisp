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

(in-package #:colander/op-specs)

(defcode! arg-spec
  `(defclass arg-spec ()
     ((name :accessor arg-name :initarg :name)
      (many? :accessor arg-many? :initarg :many? :initform nil))))
(defcode! des-spec
  `(defclass des-spec ()
     ((string :accessor des-string :initarg :string))))
(defcode! opt-spec
  `(defclass opt-spec ()
     ((name :accessor opt-name :initarg :name :initform nil)
      (short :accessor opt-short :initarg :short)
      (arg? :accessor opt-arg? :initarg :arg? :initform nil)
      (arg-name :accessor opt-arg-name :initarg :arg-name :initform nil)
      (many? :accessor opt-many? :initarg :many? :initform nil))))

(defmethod similar-p ((obj1 arg-spec) (obj2 arg-spec))
  (eq (arg-name obj1) (arg-name obj2)))

(defmethod similar-p ((obj1 des-spec) (obj2 des-spec))
  (string= (des-string obj1) (des-string obj2)))

(defmethod similar-p ((obj1 opt-spec) (obj2 opt-spec))
  ;; TODO: This will need to get rewritten after adding more functionality to
  ;;       our options.
  (and (string= (opt-short obj1) (opt-short obj2))
       (not (alexandria:xor (opt-arg? obj1) (opt-arg? obj2)))))

;;; We need a READable representation of our SPEC objects, since we'll
;;; generate macro bodies containing them as literals.
(defcode! arg-spec-load-form
  `(defmethod make-load-form ((obj arg-spec) &optional environment)
     (declare (ignorable environment))
     ;; Due to backquote not having a consistent internal representation
     ;; across implementations, this template has to be written manually...
     (reintern-to-package
      (list 'make-instance (list 'quote (type-of obj))
            :name (arg-name obj)
            :many? (arg-many? obj)))))
(defcode! des-spec-load-form
  `(defmethod make-load-form ((obj des-spec) &optional environment)
     (declare (ignorable environment))
     (reintern-to-package
      (list 'make-instance (list 'quote (type-of obj))
            :string (des-string obj)))))
(defcode! opt-spec-load-form
  `(defmethod make-load-form ((obj opt-spec) &optional environment)
     (declare (ignorable environment))
     (reintern-to-package
      (list 'make-instance (list 'quote (type-of obj))
            :name (opt-name obj)
            :short (opt-short obj)
            :arg? (opt-arg? obj)
            :arg-name (opt-arg-name obj)
            :many? (opt-many? obj)))))

(defmethod print-object ((obj arg-spec) stream)
  (format stream "~S" (make-load-form obj)))
(defmethod print-object ((obj des-spec) stream)
  (format stream "~S" (make-load-form obj)))
(defmethod print-object ((obj opt-spec) stream)
  (format stream "~S" (make-load-form obj)))

(defun arg-spec-list-p% (obj) (symbolp obj))
(defun des-spec-list-p% (obj) (stringp obj))
(defun opt-spec-list-p% (obj)
  (and (consp obj)
       (member (car obj) '(:opt :opts :arg))))

(defun mk-arg-spec% (obj)
  (make-instance 'arg-spec :name obj))
(defun mk-des-spec% (obj)
  (make-instance 'des-spec :string obj))
(defun mk-opt-spec% (obj)
  (ecase (car obj)
    (:opt (make-instance 'opt-spec :short (cadr obj)))
    (:arg (make-instance 'opt-spec :short (cadr obj) :arg? t :arg-name (caddr obj)))))

(defun explode-opt-specs% (specs)
  (iter outer
        (for spec in specs)
        (ecase (car spec)
          ((:opt :arg) (collecting (mk-opt-spec% spec)))
          (:opts (iter (for char in-vector (subseq (cadr spec) 1))
                       (in outer (collecting (mk-opt-spec% `(:opt ,(format nil "-~C" char))))))))))

(defun normalize-spec% (spec)
  (iter (while (holdp spec))
        (let ((next (car spec)))
          (cond
            ((arg-spec-list-p% next)
             (collecting (mk-arg-spec% next))
             (setf spec (cdr spec)))
            ((des-spec-list-p% next)
             (collecting (mk-des-spec% next))
             (setf spec (cdr spec)))
            ((opt-spec-list-p% next)
             (multiple-value-bind (taken left) (take-if #'opt-spec-list-p% spec)
               (collecting (explode-opt-specs% taken))
               (setf spec left)))))))
