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

(in-package #:colander/nfa)

(defclass nfa (finite-automaton) ())

(defclass nfa-node (node) ())
(defclass nfa-start-node (nfa-node) ())
(defclass nfa-normal-node (nfa-node) ())
(defclass nfa-opt-arg-parse-node (nfa-node)
  ((opt-spec :initarg :opt-spec)))
(defclass nfa-dd-node (nfa-node) ())

(defun next-cli-spec (nfa-node prods)
  (with-slots ((item datum)) nfa-node
    (item-at-dot item prods)))
(defun node-advance (nfa-node)
  (with-slots ((item datum)) nfa-node
    (setf item (item-advance item))))

(defmethod generate-root-node ((fa-type (eql 'nfa)) prods)
  (make-instance 'nfa-start-node :datum prods))

(defmethod generate-transitions ((node nfa-start-node) prods)
  (declare (ignore prods))
  (with-slots ((prods datum)) node
    (iter (for i index-of-vector prods)
          (collecting `(nil ,(make-instance
                              'nfa-normal-node
                              :datum (make-instance
                                      'item
                                      :prod-id i
                                      :dot 0)))))))

(defmethod generate-transitions ((node nfa-normal-node) prods)
  (with-slots ((item datum)) node
    `((:double-dash ,(make-instance 'nfa-dd-node :datum item))
      ,@(let ((next (next-cli-spec node prods)))
          (etypecase next
            (arg-spec `((,next ,(make-instance 'nfa-normal-node :datum (item-advance item)))))
            (des-spec `((,next ,(make-instance 'nfa-normal-node :datum (item-advance item)))))
            (null `((:accept :accept)))
            (cons `((nil ,(make-instance 'nfa-normal-node :datum (item-advance item)))
                    ,@(iter (for opt-spec in next)
                            (if (not (opt-arg? opt-spec))
                                (collecting `(,opt-spec ,node))
                                (collecting `(,opt-spec ,(make-instance
                                                          'nfa-opt-arg-parse-node
                                                          :opt-spec opt-spec
                                                          :datum item))))))))))))

(defmethod generate-transitions ((node nfa-opt-arg-parse-node) prods)
  (declare (ignore prods))
  (with-slots ((previous datum) opt-spec) node
    (list `(,(make-instance 'arg-spec :name (opt-arg-name opt-spec))
            ,(make-instance 'nfa-normal-node :datum previous)))))

(defmethod generate-transitions ((node nfa-dd-node) prods)
  (let ((next (next-cli-spec node prods)))
    (list (with-slots ((item datum)) node
            (etypecase next
              (arg-spec `(,next ,(make-instance 'nfa-dd-node :datum (item-advance item))))
              (des-spec `(,next ,(make-instance 'nfa-dd-node :datum (item-advance item))))
              (null `(:accept :accept)))))))

(defmethod same-state-p ((node1 nfa-normal-node) (node2 nfa-normal-node))
  (similar-p (slot-value node1 'datum) (slot-value node2 'datum)))
(defmethod same-state-p ((node1 nfa-opt-arg-parse-node) (node2 nfa-opt-arg-parse-node))
  (and (similar-p (slot-value node1 'opt-spec) (slot-value node2 'opt-spec))
       (similar-p (slot-value node1 'datum) (slot-value node2 'datum))))
(defmethod same-state-p ((node1 nfa-dd-node) (node2 nfa-dd-node))
  (similar-p (slot-value node1 'datum) (slot-value node2 'datum)))

(defmethod initialize-node ((fa-node nfa-dd-node) prods)
  (with-slots ((item datum)) fa-node
    (let ((next (make-instance 'nfa-dd-node :datum item)))
      (with-slots ((item datum)) next
        (iter (while (consp (next-cli-spec next prods)))
              (setf item (item-advance item)))
        next))))

(defun cli-specs-to-prods (cli-specs)
  ;; CLI-SPECS :: (LIST CLI-SPEC)
  (make-array (length cli-specs)
              :initial-contents
              (mapcar (lambda (cli-spec)
                        (make-instance 'production :cli-spec cli-spec))
                      cli-specs)))

(defun prods-to-nfa (prods)
  (generate-finite-automaton 'nfa prods))
