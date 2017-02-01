(in-package #:colander/parser)


;;; Utility functions that we need both in our generated parsers and in
;;; COLANDER itself for doing error-checking on parser specifications.

(defcodefn! identifier-char-p (char)
  (or (alphanumericp char) (find char "-_")))

(defcodefn! short-opt-p (str)
  (and (stringp str)
       (= (length str) 2)
       (char= (char str 0) #\-)
       (alphanumericp (char str 1))
       str))

(defcodefn! long-opt-p (str)
  (and (stringp str)
       (> (length str) 2)
       (string= str "--" :end1 2)
       (every #'identifier-char-p (subseq str 2))
       str))

(defcodefn! combined-short-opt-p (str)
  (and (stringp str)
       (> (length str) 2)
       (char= (char str 0) #\-)
       (every #'alphanumericp (subseq str 1))
       str))

(defcodefn! single-opt-p (str)
  (or (short-opt-p str) (long-opt-p str)))

(defcodefn! opt-p (str)
  (or (single-opt-p str) (combined-short-opt-p str)))

(defcodefn! included-arg-opt-p (str)
  (when (stringp str)
    (let ((equal (position #\= str)))
      (when equal
        (and (not (zerop (length (subseq str (1+ equal)))))
             (opt-p (subseq str 0 equal)))))))

(defcodefn! included-arg (str)
  "Extract the included argument in the option; return them if found, NIL
   if not."
  (when (included-arg-opt-p str)
    (let ((equal (position #\= str)))
      (if (funcall #'opt-p (subseq str 0 equal))
          (values (subseq str (1+ equal))
                  (subseq str 0 equal))
          (values nil nil)))))

(defcodefn! expand-short (opt)
  (loop for char across (subseq opt 1)
        collect (format nil "-~C" char)))

(defcodefn! maybe-expand (opt)
  (if (long-opt-p opt)
      (list opt)
      (expand-short opt)))

(defcodefn! fully-expand-args (arg)
  (multiple-value-bind (arg* opt*) (included-arg arg)
    (if arg*
        (append (maybe-expand opt*) (list arg*))
        (maybe-expand arg))))

(defcodefn! normalize-args (args)
  (loop for arg in args
        with double-dash = nil
        if (not double-dash)
          if (string= arg "--")
            do (setf double-dash t)
            and collect arg
          else if (not (or (opt-p arg) (included-arg-opt-p arg)))
            collect arg
          else
            append (fully-expand-args arg)
          end
        else
          collect arg
        end))


;;; Code specific to the generated parsers.

(defcode prod-symbol (prod-id)
  (symb "prod" prod-id))

(defcode prod (prod prod-id)
  `(defparameter ,(generate-code 'prod-symbol prod-id)
     (list ,@(mapcar (lambda (obj) (if (listp obj) (cons 'list obj) obj))
                     (slot-value prod 'cli-spec)))))

(defcode prods (prods)
  (iter (for id index-of-vector prods)
        (for prod in-vector prods)
        (collecting (generate-code 'prod prod id))))

(defcode dfa-state-symbol (dfa-node)
  (symb "state" (slot-value dfa-node 'id)))

(defcode arg-parse-driver (dfa)
  `(defun arg-parse-driver (tokens)
     (let ((state (function ,(generate-code 'dfa-state-symbol (slot-value dfa 'root)))))
       (dolist (token tokens)
         (setf state (funcall state token)))
       (funcall state nil))))

;; We add DECLAIMs to our generated parser so that SBCL doesn't whine about
;; undeclared functions in our mutual recursion.
(defcode dfa-state-declaim (dfa-node)
  `(declaim (ftype function ,(generate-code 'dfa-state-symbol dfa-node))))

;; ACCEPT transitions.
(defmethod generate-code ((states list) &rest args)
  (destructuring-bind (out) args
    (declare (ignorable out))
    `((null token)
      ,(if (not (singlep states))
           `(error "Accept/accept conflict. Parse failure.")
           (generate-code 'prod-symbol (slot-value (slot-value (car states) 'datum) 'prod-id))))))

(defmethod generate-code ((code-name (eql :double-dash)) &rest args)
  (destructuring-bind (out) args
    `((string= "--" token)
      (function ,(generate-code 'dfa-state-symbol out)))))

(defmethod generate-code ((code-name arg-spec) &rest args)
  (declare (ignorable code-name))
  (destructuring-bind (out) args
    `((not (or (short-opt-p token) (long-opt-p token) (string= token "--")))
      (function ,(generate-code 'dfa-state-symbol out)))))

(defmethod generate-code ((code-name des-spec) &rest args)
  (destructuring-bind (out) args
    `((string= token ,(des-string code-name))
      (function ,(generate-code 'dfa-state-symbol out)))))

(defmethod generate-code ((code-name opt-spec) &rest args)
  (destructuring-bind (out) args
    ;; TODO: More than just short opt strings.
    `((and (or (short-opt-p token) (long-opt-p token))
           (string= token ,(opt-short code-name)))
      (function ,(generate-code 'dfa-state-symbol out)))))

(defcode dfa-state (dfa-node)
  `(defun ,(generate-code 'dfa-state-symbol dfa-node) (token)
     (cond
       ,@(iter (for transition in (slot-value dfa-node 'edges))
               (with-slots (label out) transition
                 (collecting (generate-code label out))))
       (:otherwise
        (error "No transition for ~S from state ~A." token ,(slot-value dfa-node 'id))))))

;; Our actual parser! Surprisingly simple, since we don't need any error-checking;
;; that's all been handled by running our tokens through our DFA.

(defcode parse-state ()
  `(defclass parse-state ()
     ((tokens :initarg :tokens :accessor parse-tokens)
      (specs :initarg :specs :accessor parse-specs)
      (parsed :initarg :parsed :initform '()  :accessor parse-parsed)
      (dd? :initarg :dd? :initform nil :accessor parse-dd?))))

(defcode parse-cli-spec ()
  `(defun parse-cli-spec (tokens spec)
     (loop with state = (make-instance 'parse-state :tokens tokens :specs spec)
           while (consp (parse-specs state))
           do (setf state (delegate-parse-state-transformer state))
           finally (return (nreverse (parse-parsed state))))))

(defcode normalize-parse-state ()
  `(defun normalize-parse-state (parse-state)
     (cond
       ((and (parse-dd? parse-state) (listp (first (parse-specs parse-state))))
        (setf (parse-specs parse-state) (rest (parse-specs parse-state)))
        parse-state)
       (:otherwise parse-state))))

(defcode parsing-function-declaims ()
  `(declaim (ftype function arg-parse-driver)))

(defcode delegate-parse-state-transformer ()
  `(defun delegate-parse-state-transformer (parse-state)
     (when (and (not (parse-dd? parse-state))
                (string= "--" (first (parse-tokens parse-state))))
       (setf (parse-tokens parse-state) (rest (parse-tokens parse-state)))
       (setf (parse-dd? parse-state) t))
     (let ((parse-state (normalize-parse-state parse-state)))
       (funcall (etypecase (first (parse-specs parse-state))
                  (arg-spec #'parse-arg-state)
                  (des-spec #'parse-des-state)
                  (cons #'parse-opt-state))
                parse-state))))

(defcode parse-arg-state ()
  `(defun parse-arg-state (parse-state)
     (let ((arg-spec (pop (parse-specs parse-state)))
           (token (pop (parse-tokens parse-state))))
       (push (list (arg-name arg-spec) token)
             (parse-parsed parse-state))
       parse-state)))

(defcode parse-des-state ()
  `(defun parse-des-state (parse-state)
     (let ((des-spec (pop (parse-specs parse-state)))
           (token (pop (parse-tokens parse-state))))
       (declare (ignorable des-spec token))
       parse-state)))

(defcode parse-opt-state ()
  `(defun parse-opt-state (parse-state)
     (let ((opt-specs (first (parse-specs parse-state)))
           (token (pop (parse-tokens parse-state))))
       (cond
         ((not (or (short-opt-p token) (long-opt-p token)))
          (pop (parse-specs parse-state))
          (push token (parse-tokens parse-state))
          parse-state)
         (:otherwise
          ;; TODO: More than just the short option name.
          (let ((opt-spec (find token opt-specs :test #'string= :key #'opt-short)))
            (cond
              ((opt-arg? opt-spec)
               (push (make-instance 'arg-spec :name (opt-short opt-spec))
                     (parse-specs parse-state))
               parse-state)
              (:otherwise
               (push (list (opt-short opt-spec) t)
                     (parse-parsed parse-state))
               parse-state))))))))

(defcode parse ()
  `(defun parse (&optional (tokens (cdr (argv))))
     (let ((tokens (normalize-args tokens)))
       (parse-cli-spec tokens (arg-parse-driver tokens)))))
