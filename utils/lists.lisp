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

(defun holdp (list) (not (null list)))

(defun singlep (list) (and (not (cdr list)) list))


(defun take (n list)
  "Collect N elements from the front of LIST. Second return value is the
   rest of the list after stopping."
  (do* ((list list (cdr list))
        (element (car list) (car list))
        (i 0 (1+ i))
        (acc '() (cons element acc)))
       ((or (null list) (= i n))
        (values (nreverse acc) list))))

(defun drop (n list)
  "Remove N elements from the front of LIST."
  (multiple-value-bind (front back) (take n list)
    (declare (ignore front))
    back))

(defun take-if (fn list)
  "Collect elements from the front of LIST, until finding one which does
   not satisfy the predicate FN. Second return value is the rest of the
   list after stopping."
  (do* ((list list (cdr list))
        (acc '() (cons element acc))
        (element (car list) (car list)))
       ((or (null list) (not (funcall fn element)))
        (values (nreverse acc) list))))

(defun drop-if (fn list)
  "Remove elements from LIST until finding one which does not
   satisfy the predicate FN."
  (multiple-value-bind (outside collected) (take-if fn list)
    (declare (ignore outside))
    collected))


(defun segregate-binary (list fn)
  "Like SEGREGATE, but only takes a single predicate FN, and the primary
   return value is simply all elements which satisfied FN."
  (labels ((segregate-binary* (remain collect outside)
             (if (null remain)
                 (values (nreverse collect) (nreverse outside))
                 (if (funcall fn (car remain))
                     (segregate-binary* (cdr remain) (cons (car remain) collect) outside)
                     (segregate-binary* (cdr remain) collect (cons (car remain) outside))))))
    (segregate-binary* list '() '())))

(defun segregate (list &rest fns)
  "Take a list of predicates FNS, return a list of lists consisting of
   the elements which satisfied each FN. Each FN is expected to be
   disjoint from all the others, and SEGREGATE will return a *partition*
   of the original list. If they're not disjoint, an element which satisfies
   multiple predicates will only be placed in the list of the leftmost FN.

   Secondary return value is a list of all elements which failed to satisfy
   any predicate."
  (labels ((segregate* (remain collect fns)
             (if (null fns)
                 (values (nreverse collect) remain)
                 (multiple-value-bind (collect* remain*)
                     (segregate-binary remain (car fns))
                   (segregate* remain* (cons collect* collect) (rest fns))))))
    (segregate* list '() fns)))


(defun flatten (list)
  (alexandria:mappend
   (lambda (x) (if (listp x) (flatten x) (list x)))
   list))

(defun group (n source)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))
