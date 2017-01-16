(in-package #:colander)

;; First things first -- essentially we want to handle the case of
;; a single, consistent interface, where there's only one way to call our
;; utility.

(defun argv ()
  "Returns list of command-line arguments, including the name of executable."
  #+abcl ext:*command-line-argument-list*
  #+allegro sys:command-line-arguments
  #+ccl ccl:*command-line-argument-list*
  #+clisp (cons *load-truename* ext:*args*)
  #+clozure ccl::command-line-arguments
  #+cmu extensions:*command-line-words*
  #+ecl (ext:command-args)
  #+gcl si:*command-args*
  #+lispworks system:*line-arguments-list*
  #+sbcl sb-ext:*posix-argv*)

(defmacro alias (name fn)
  "Set the SYMBOL-FUNCTION of NAME correctly. FN should be a form
   which evaluates to a function."
  `(progn
     (declaim (ftype function ,name))
     (setf (symbol-function ',name) ,fn)
     ',name))

(alias alpha-number-p #'alphanumericp)

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
         (:many (and (eq (caadr opt-spec) :arg)
                     (verify-opt-spec (cadr opt-spec))))
         (:opts (and (rest opt-spec)
                     (every (lambda (x)
                              (cond
                                ((stringp x) (opt-p x))
                                ((consp x) (verify-opt-spec x))))
                            (rest opt-spec)))))
       opt-spec))

(defun opt-name (opt)
  (subseq opt (cond ((long-opt-p opt) 2)
                    ((short-opt-p opt) 1)
                    (:otherwise (error "Not an option: ~S" opt)))))

(defun explode-opt-spec (opt-spec)
  (when (verify-opt-spec opt-spec)
    (ecase (car opt-spec)
      (:opt (list (cadr opt-spec)))
      (:arg (list opt-spec))
      (:many (list opt-spec))
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
       (or (member (car obj) '(:opt :arg :opts))
           (and (eq (car obj) :many)
                (opt-spec-p (cadr obj))))))
(alias arg-spec-p (one-of #'symbolp
                          (lambda (x)
                            (and (consp x)
                                 (eq (car x) :many)
                                 (symbolp (cadr x))))))
(alias designator-spec-p #'stringp)

(defun verify-spec (spec)
  (and (listp spec)
       (every (one-of #'arg-spec-p
                      #'designator-spec-p
                      (all-of #'opt-spec-p #'verify-opt-spec))
              spec)
       (every (lambda (x)
                (funcall (one-of #'designator-spec-p
                                 #'opt-spec-p
                                 #'null)
                         (second x)))
              (remove-if-not (lambda (x) (arg-spec-p (car x)))
                             (suffixes spec)))))

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
                        (recur rest (cons (mappend #'explode-opt-spec opts) acc)))))))))
    (recur spec '())))



