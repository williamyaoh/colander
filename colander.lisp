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

(defmacro alias (name fn)
  "Set the SYMBOL-FUNCTION of NAME correctly. FN should be a form
   which evaluates to a function."
  `(progn
     (declaim (ftype function ,name))
     (setf (symbol-function ',name) ,fn)
     ',name))

(alias alpha-number-p #'alphanumericp)

(defun one-of (fn &rest fns)
  (lambda (x)
    (and (some (lambda (f) (funcall f x))
               (cons fn fns))
         x)))

(alias identifier-char-p (one-of #'alpha-number-p
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

(alias single-opt-p (one-of #'long-opt-p #'short-opt-p))
(alias opt-p (one-of #'single-opt-p #'combined-short-opt-p))

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
(alias arg-spec-p (all-of #'stringp (complement #'opt-p)))
(alias designator-spec-p #'symbolp)

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

(defun verify-spec (spec)
  (and (listp spec)
       (every (one-of #'arg-spec-p
                      #'designator-spec-p
                      (all-of #'opt-spec-p #'verify-opt-spec))
              spec)))

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

(defun fold-spec (spec)
  "Take a single specification, fold together all the adjacent options."
  (labels ((recur (spec acc)
             (if (null spec)
                 (nreverse acc)
                 (let ((spec* (car spec)))
                   (cond
                     ((or (arg-spec-p spec*) (designator-spec-p spec*))
                      (recur (rest spec) (cons spec* acc)))
                     ((opt-spec-p spec*)
                      (multiple-value-bind (opts rest) (take-if #'opt-spec-p spec)
                        (recur rest
                               (cons (mappend #'explode-opt-spec opts) acc)))))))))
    (recur spec '())))
