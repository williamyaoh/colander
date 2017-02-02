;;; Modified from mrkkrp's UNIX-OPTS.
;;; <https://github.com/mrkkrp/unix-opts/blob/master/unix-opts.lisp>

;;; Copyright (c) 2015–2017 Mark Karpov
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

(defun argv ()
  "Returns list of command-line arguments, including the name of the executable."
  #+abcl ext:*command-line-argument-list*
  #+allegro sys:command-line-arguments
  #+ccl ccl:*command-line-argument-list*
  #+clisp (cons *load-truename* ext:*args*)
  #+clozure ccl::command-line-arguments
  #+cmu extensions:*command-line-words*
  #+ecl (ext:command-args)
  #+gcl si:*command-args*
  #+lispworks system:*line-arguments-list*
  #+sbcl sb-ext:*posix-argv*
  #-(or abcl allegro ccl clisp clozure cmu ecl gcl lispworks sbcl)
  #.(error "Don't know how to get ARGV in current Lisp implementation."))
