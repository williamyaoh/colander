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
  '("new" "foo" :name (:arg "--loop" "LOOPAMT")
    (:opts "-abcdEf" "--blah" (:arg "--int" "INT")) (:opt "--lol")
    :something))

(setf (symbol-function 'alpha-number-p) #'alphanumericp)

(defun one-of (fn &rest fns)
  (lambda (x)
    (and (some (lambda (f) (funcall f x))
               (cons fn fns))
         x)))

(setf (symbol-function 'identifier-char-p)
      (one-of #'alpha-number-p
              (lambda (c)
                (find c "-_"))))

(defun long-opt-p (str)
  (and (stringp str)
       (> (length str) 2)
       (string= "--" str :end2 2)
       (every #'identifier-char-p (subseq str 2))
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

(setf (symbol-function 'single-opt-p) (one-of #'long-opt-p #'short-opt-p))

(setf (symbol-function 'opt-p) (one-of #'single-opt-p #'combined-short-opt-p))

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
        (if (funcall #'single-opt-p (subseq str 0 equal))
            (values (subseq str (1+ equal))
                    (subseq str 0 equal))
            (values nil nil))))))

(defun verify-opt-spec (opt-spec)
  (and (ecase (car opt-spec)
         (:opt (funcall #'single-opt-p (cadr opt-spec)))
         (:arg (and (funcall #'single-opt-p (cadr opt-spec))
                    (funcall (one-of #'stringp #'symbolp) (caddr opt-spec))))
         (:opts (and (rest opt-spec)
                     (every (lambda (x)
                              (cond
                                ((stringp x) (opt-p x))
                                ((consp x) (verify-opt-spec x))))
                            (rest opt-spec)))))
       opt-spec))

(defun mappend (fn seq &rest more-seqs)
  (loop for list in (apply #'map 'list fn (cons seq more-seqs))
        append list))

(defun opt-name (opt)
  (subseq opt (cond ((long-opt-p opt) 2)
                    ((short-opt-p opt) 1)
                    (:otherwise (error "Not an option: ~S" opt)))))

(defun explode-opt-spec (opt-spec)
  (when (verify-opt-spec opt-spec)
    (ecase (car opt-spec)
      (:opt (list (cadr opt-spec)))
      (:arg (list opt-spec))
      (:opts (mappend (lambda (x)
                        (cond
                          ((consp x) (explode-opt-spec x))
                          ((single-opt-p x) (list x))
                          ((combined-short-opt-p x)
                           (map 'list (lambda (c)
                                        (concatenate 'string "-" (string c)))
                                (subseq x 1)))))
                      (rest opt-spec))))))

(defun opt-spec-p (obj)
  (and (consp obj)
       (member (car obj) '(:opt :arg :opts))))
(setf (symbol-function 'arg-spec-p)
      (all-of #'stringp (complement #'opt-p)))
(setf (symbol-function 'designator-spec-p) #'symbolp)

(defun fold-spec (spec)
  "Take a single specification, fold together all the adjacent options."
  )
