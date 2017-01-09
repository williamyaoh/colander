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

(defparameter *test-spec*
  '("new" "foo" (:arg :name) (:arg "--loop")))

(setf (symbol-function 'alpha-number-p) #'alphanumericp)

(defun long-opt-p (str)
  (and (stringp str)
       (> (length str) 2)
       (string= "--" str :end2 2)
       (every #'alpha-number-p (subseq str 2))
       str))

(defun short-opt-p (str)
  (and (stringp str)
       (= (length str) 2)
       (char= (char str 0) #\-)
       (alpha-number-p (char str 1))
       str))

(defun combined-short-opt-p (str)
  (and (stringp str)
       (> (length str) 2)
       (char= (char str 0) #\-)
       (every #'alpha-number-p (subseq str 1))
       str))

(defun one-of (fn &rest fns)
  (lambda (x)
    (and (some (lambda (f) (funcall f x))
               (cons fn fns))
         x)))

(defun all-of (fn &rest fns)
  (lambda (x)
    (and (every (lambda (f) (funcall f x))
                (cons fn fns))
         x)))

(defun included-arg (str)
  "Extract the included argument in the option; return them if found, NIL
   if not."
  (when (stringp str)
    (let ((equal (position #\= str)))
      (when equal
        (if (funcall (one-of #'long-opt-p #'short-opt-p)
                     (subseq str 0 equal))
            (values (subseq str (1+ equal))
                    (subseq str 0 equal))
            (values nil nil))))))

