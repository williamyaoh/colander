;;; Modified from Fare's UIOP.
;;; <https://github.com/fare/asdf/blob/master/uiop/utility.lisp>

;;; Copyright (c) 2001-2017 Francois-Rene Rideau
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defun finish-outputs ()
  (dolist (stream (list *standard-output* *error-output* *trace-output* *debug-io* *terminal-io* *query-io*))
    (ignore-errors (finish-output stream)))
  (values))

(defun quit (&optional (code 0) (finish-output t))
  (when finish-output (finish-outputs))
  #+(or abcl xcl) (ext:quit :status code)
  #+allegro (excl:exit code :quiet t)
  #+(or clasp ecl) (si:quit code)
  #+clisp (ext:quit code)
  #+clozure (ccl:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+(or cmucl scl) (unix:unix-exit code)
  #+gcl (system:quit code)
  #+genera (error "~S: You probably don't want to halt Genera. (code: ~S)" 'quit code)
  #+lispworks (lispworks:quit :status code :confirm nil :return nil :ignore-errors-p t)
  #+mcl (progn code (ccl:quit))
  #+mkcl (mk-ext:quit :exit-code code)
  #+sbcl #.(let ((exit (and (find-package "SB-EXT") (find-symbol "EXIT" (find-package "SB-EXT"))))
                 (quit (and (find-package "SB-EXT") (find-symbol "QUIT" (find-package "SB-EXT")))))
             (cond
               (exit `(,exit :code code :abort (not finish-output)))
               (quit `(,quit :unix-status code :recklessly-p (not finish-output)))))
  #-(or abcl allegro clasp clisp clozure cmucl ecl gcl genera lispworks mcl mkcl sbcl scl xcl)
  (error "~S called with exit code ~S, but there's no quitting on this implementation." 'quit code))
