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

(defun slurp (filename)
  "Read the contents of a file into a string."
  (with-open-file (input filename
                         :direction :input
                         :element-type 'character)
    (with-output-to-string (output)
      (iter (for char = (read-char input nil nil))
            (while char)
            (write-char char output)))))

(defun spit (string filename &optional (append? nil))
  "Print out the contents of the string into the file."
  (with-open-file (output filename
                          :direction :output
                          :element-type 'character
                          :if-does-not-exist :create
                          :if-exists (if append? :append :overwrite))
    (write-string string output)))

(defun relative-pathname (filename)
  (asdf:system-relative-pathname '#:colander filename))

(defun lines (input)
  "If INPUT is a string, return a list of its lines. If INPUT is a STREAM,
   read all its lines into a list."
  (with-open-stream (*standard-input*
                     (if (streamp input)
                         input
                         (make-string-input-stream input)))
    (iter (for line = (read-line *standard-input* nil nil))
          (while line)
          (collecting line))))

(defun unlines (lines)
  "Concatenate all the lines together into a string, with newlines in between
   them. Has a trailing newline."
  (format nil "~{~A~%~}" lines))

(defun trim-comment (line)
  (let ((semicolon (position #\; line)))
    (if semicolon
        (subseq line 0 semicolon)
        line)))

(defun trim-trailing-whitespace (line)
  (string-right-trim
   #.(format nil "~{~C~}" (mapcar #'code-char '(#x09 #x0A #x0B #x0C #x0D #x20)))
   line))

(defun squish-invisibles (input)
  "Squish together empty lines and comments in Lisp-ish files."
  (unlines
   (remove-if
    (lambda (line) (zerop (length line)))
    (mapcar (lambda (line) (trim-trailing-whitespace (trim-comment line)))
            (lines input)))))
