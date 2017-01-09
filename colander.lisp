(in-package #:colander)

;; First things first -- essentially we want to handle the case of
;; a single, consistent interface, where there's only one way to call our
;; utility.

(defun argv ()
  "Returns list of command-line arguments, including the name of executable."
  #+abcl      ext:*command-line-argument-list*
  #+allegro   sys:command-line-arguments
  #+ccl       ccl:*command-line-argument-list*
  #+clisp     (cons *load-truename* ext:*args*)
  #+clozure   ccl::command-line-arguments
  #+cmu       extensions:*command-line-words*
  #+ecl       (ext:command-args)
  #+gcl       si:*command-args*
  #+lispworks system:*line-arguments-list*
  #+sbcl      sb-ext:*posix-argv*)
