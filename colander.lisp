(in-package #:colander)

(defgeneric generate-code (code-name &rest args))

;; We use REINTERN-TO-PACKAGE within the definition of DEFCODE below,
;; but we also need it in our output code.
(defmethod generate-code ((code-name (eql 'reintern-to-package)) &rest args)
  (declare (ignorable args))
  `(defun reintern-to-package (form &optional (package *package*))
     (typecase form
       (cons (map 'list (lambda (form) (reintern-to-package form package)) form))
       (symbol (if (or (keywordp form) (not (symbol-package form)))
                   form
                   (intern (symbol-name form) package)))
       (otherwise form))))

(eval (generate-code 'reintern-to-package))

(defmethod generate-code :around ((code-name (eql 'reintern-to-package)) &rest args)
  (declare (ignorable args))
  (reintern-to-package (call-next-method)))

(defmacro defcode (name (&rest code-fn-args) &body body)
  "BDDY should return some Lisp code; DEFCODE provides a generic function to
   automatically reintern said code in a different package."
  (let ((code-name (gensym "CODE-NAME"))
        (inner-args (gensym "ARGS")))
    `(defmethod generate-code ((,code-name (eql ',name)) &rest ,inner-args)
       (destructuring-bind ,code-fn-args ,inner-args
         (reintern-to-package
          (progn ,@body))))))

(defmacro defcode! (name &body body)
  "Used for defining code that's useful both in the current package
   and for generating somewhere else."
  `(progn (defcode ,name () ,@body)
          (eval (generate-code ',name))))

(defmacro defcodefn! (name (&rest args) &body body)
  "Used for defining functions which are useful both in the current package
   and for generating somewhere else."
  `(defcode! ,name '(defun ,name ,args ,@body)))

;; Firstly, a standard syntax for our specifications.
;; This syntax needs to be readable, because we need to be able
;; to copy it into our generated parser...

(defcode! arg-spec
  `(defclass arg-spec ()
     ((name :accessor arg-name :initarg :name)
      (many? :accessor arg-many? :initarg :many? :initform nil))))
(defcode! des-spec
  `(defclass des-spec ()
     ((string :accessor des-string :initarg :string))))
(defcode! opt-spec
  `(defclass opt-spec ()
     ((name :accessor opt-name :initarg :name)
      (short :accessor opt-short :initarg :short)
      (arg? :accessor opt-arg? :initarg :arg? :initform nil)
      (arg-name :accessor opt-arg-name :initarg :arg-name :initform nil)
      (many? :accessor opt-many? :initarg :many? :initform nil))))

(defcode! arg-spec-load-form
  `(defmethod make-load-form ((obj arg-spec) &optional environment)
     (declare (ignorable environment))
     ;; Due to backquote not having a consistent internal representation
     ;; across implementations, this template has to be written manually...
     (reintern-to-package
      (list 'make-instance (list 'quote (type-of obj))
            :name (arg-name obj)
            :many? (arg-many? obj)))))
(defcode! des-spec-load-form
  `(defmethod make-load-form ((obj des-spec) &optional environment)
     (declare (ignorable environment))
     (reintern-to-package
      (list 'make-instance (list 'quote (type-of obj))
            :string (des-string obj)))))
(defcode! opt-spec-load-form
  `(defmethod make-load-form ((obj opt-spec) &optional environment)
     (declare (ignorable environment))
     (reintern-to-package
      (list 'make-instance (list 'quote (type-of obj))
            :name (opt-name obj)
            :short (opt-short obj)
            :arg? (opt-arg? obj)
            :arg-name (opt-arg-name obj)
            :many? (opt-many? obj)))))

(defmethod print-object ((obj arg-spec) stream)
  (format stream "~S" (make-load-form obj)))
(defmethod print-object ((obj des-spec) stream)
  (format stream "~S" (make-load-form obj)))
(defmethod print-object ((obj opt-spec) stream)
  (format stream "~S" (make-load-form obj)))

(defun arg-spec-list-p% (obj) (symbolp obj))
(defun des-spec-list-p% (obj) (stringp obj))
(defun opt-spec-list-p% (obj)
  (and (consp obj)
       (member (car obj) '(:opt :opts :arg))))

(defun mk-arg-spec% (obj)
  (make-instance 'arg-spec :name obj))
(defun mk-des-spec% (obj)
  (make-instance 'des-spec :string obj))
(defun mk-opt-spec% (obj)
  (ecase (car obj)
    (:opt (make-instance 'opt-spec :short (cadr obj)))
    (:arg (make-instance 'opt-spec :short (cadr obj) :arg? t :arg-name (caddr obj)))))

(defun explode-opt-specs% (specs)
  (iter outer
        (for spec in specs)
        (ecase (car spec)
          ((:opt :arg) (collecting (mk-opt-spec% spec)))
          (:opts (iter (for char in-vector (subseq (cadr spec) 1))
                       (in outer (collecting (mk-opt-spec% `(:opt ,(format nil "-~C" char))))))))))

(defun normalize-spec% (spec)
  (iter (while (holdp spec))
        (let ((next (car spec)))
          (cond
            ((arg-spec-list-p% next)
             (collecting (mk-arg-spec% next))
             (setf spec (cdr spec)))
            ((des-spec-list-p% next)
             (collecting (mk-des-spec% next))
             (setf spec (cdr spec)))
            ((opt-spec-list-p% next)
             (multiple-value-bind (taken left) (take-if #'opt-spec-list-p% spec)
               (collecting (explode-opt-specs% taken))
               (setf spec left)))))))


;; NFA.

(defvar *nfa*)
(defvar *nfa-computed-transitions*)

(defclass production ()
  ((cli-spec :initarg :cli-spec)))
(defclass item ()
  ((prod-id :initarg :prod-id)
   (dot :initarg dot)))

(defun item-advance (item)
  (with-slots (dot) item
    (incf dot)))
(defun item-prod (item)
  (lookup-prod (slot-value item 'prod-id)))
(defun item-at-dot (item)
  (with-slots (dot) item
    (nth dot (item-prod item))))

(defclass transition ()
  ((edge :accessor transition-edge :initarg :edge)
   (out :accessor transition-out :initarg :out)))
(defclass nfa-node ()
  ((id :accessor nfa-node-id :initarg :id)
   (datum :accessor nfa-node-datum :initarg :datum)
   (transitions :accessor nfa-node-transitions :initarg :transitions :initform '())))
(defclass nfa ()
  ((root :accessor nfa-root :initarg :root)
   (lookup :accessor nfa-lookup :initarg :lookup
           :initform (make-array 5 :adjustable t :fill-pointer 0))))

(defclass nfa-accept-node (nfa-node) ())

(defgeneric same-state (obj1 obj2)
  (:documentation
   "Used to determine if a transition or node has been seen before
    (so we can cache it when necessary)")
  (:method (obj1 obj2) nil))

(defclass nfa-normal-parse-node (nfa-node) ())
(defclass nfa-parse-arg-node (nfa-node)
  ((previous :accessor parse-arg-previous :initarg :previous)))
(defclass nfa-arg-only-node (nfa-node) ())

(defmethod same-state ((obj1 opt-spec) (obj2 opt-spec))
  (eq obj1 obj2))

(defmethod same-state ((obj1 nfa-normal-parse-node) (obj2 nfa-normal-parse-node))
  (eq (nfa-node-datum obj1) (nfa-node-datum obj2)))
(defmethod same-state ((obj1 nfa-parse-arg-node) (obj2 nfa-parse-arg-node))
  (and (eq (parse-arg-previous obj1) (parse-arg-previous obj2))
       (same-state (nfa-node-datum obj1) (nfa-node-datum obj2))))
(defmethod same-state ((obj1 nfa-arg-only-node) (obj2 nfa-arg-only-node))
  (eq (nfa-node-datum obj1) (nfa-node-datum obj2)))

(defun lookup-state (state) (aref (nfa-lookup *nfa*) state))
(defun lookup-datum (state) (nfa-node-datum (lookup-state state)))

(defun intern-nfa-state (state)
  (let ((old-state (iter (for id index-of-vector (nfa-lookup *nfa*))
                         (finding id such-that (same-state state (lookup-state id))))))
    (if old-state
        old-state
        (setf (nfa-node-id state) (vector-push-extend state (nfa-lookup *nfa*))))))

;; (defun generate-new-nfa-state (type datum &rest keys)
;;   ;; EQ is sufficient for now. Need better checking in the future.
;;   (let ((old-state (iter (for state in (alexandria:hash-table-keys (nfa-lookup *nfa*)))
;;                          (finding state such-that (eq (lookup-datum state) datum)))))
;;     (if old-state
;;         old-state
;;         (let ((new-state (apply #'make-instance type :datum datum keys)))
;;           (vector-push-extend new-state (nfa-lookup *nfa*))))))

(defun spec-compute-specs-nfa (specs)
  "Determine an NFA from the specs."
  (let* ((*nfa* (make-instance 'nfa))
         (*nfa-computed-transitions* (make-hash-table))
         (root-node (make-instance 'nfa-node :datum nil)))
    (intern-nfa-state root-node)
    (setf (nfa-root *nfa*) root-node)
    (iter (for spec in specs)
          (push (make-instance 'transition
                               :edge nil
                               :out (intern-nfa-state
                                     (make-instance 'nfa-normal-parse-node
                                                    :datum spec)))
                (nfa-node-transitions root-node)))
    (iter (for transition in (nfa-node-transitions root-node))
          (spec-compute-transitions-nfa (transition-out transition)))
    *nfa*))

(defun spec-compute-transitions-nfa (state)
  (when (not (gethash state *nfa-computed-transitions*))
    (setf (gethash state *nfa-computed-transitions*) t)
    (let* ((node (aref (nfa-lookup *nfa*) state))
           (transitions (compute-transitions% node)))
      (iter (for (edge state) in transitions)
            (push (make-instance 'transition :edge edge :out state)
                  (nfa-node-transitions node)))
      (iter (for (edge state) in transitions)
            (spec-compute-transitions-nfa state)))))

(defgeneric compute-transitions% (node))

(defmethod compute-transitions% ((node nfa-normal-parse-node))
  (let* ((datum (nfa-node-datum node))
         (next (car datum)))
    `((double-dash
       ,(intern-nfa-state
         (make-instance 'nfa-arg-only-node
                        :datum (etypecase next
                                 (arg-spec datum)
                                 (des-spec datum)
                                 (null datum)
                                 (cons (cdr datum))))))
      ,@(etypecase next
          (arg-spec
           `((,next
              ,(intern-nfa-state
                (make-instance 'nfa-normal-parse-node :datum (cdr datum))))))
          (des-spec
           `((,next
              ,(intern-nfa-state
                (make-instance 'nfa-normal-parse-node :datum (cdr datum))))))
          (null `((accept ,(intern-nfa-state (make-instance 'nfa-accept-node)))))
          (cons
           (cons
            `(nil ,(intern-nfa-state
                    (make-instance 'nfa-normal-parse-node :datum (cdr datum))))
            (iter (for opt-spec in next)
                  (if (not (opt-arg? opt-spec))
                      (collecting `(,opt-spec ,(nfa-node-id node)))
                      (collecting `(,opt-spec
                                    ,(intern-nfa-state
                                      (make-instance 'nfa-parse-arg-node
                                                     :datum opt-spec
                                                     :previous (nfa-node-id node)))))))))))))
(defmethod compute-transitions% ((node nfa-parse-arg-node))
  ;; We got here from an OPT-SPEC that has an arg, so the only outgoing edge
  ;; should be if we get an argument.
  `((,(make-instance 'arg-spec :name (opt-arg-name (nfa-node-datum node)))
     ,(parse-arg-previous node))))
(defmethod compute-transitions% ((node nfa-arg-only-node))
  (let* ((datum (nfa-node-datum node))
         (next (car datum)))
    (etypecase next
      (arg-spec `((,next ,(intern-nfa-state
                           (make-instance 'nfa-arg-only-node
                                          :datum (drop-if #'listp (cdr datum)))))))
      (des-spec `((,next ,(intern-nfa-state
                           (make-instance 'nfa-arg-only-node
                                          :datum (drop-if #'listp (cdr datum)))))))
      (null `((accept ,(intern-nfa-state (make-instance 'nfa-accept-node))))))))
(defmethod compute-transitions% ((node nfa-accept-node)) '())


;; DFA.

(defun epsilon-closure (state)
  (cons state (iter (for transition in (nfa-node-transitions state))
                    (when (null (transition-edge transition))
                      (appending
                       (epsilon-closure (lookup-state (transition-out transition))))))))

(defun epsilon-closures (states)
  (iter (for state in states) (unioning (epsilon-closure state))))

(defclass dfa-node ()
  ((node-id :accessor dfa-node-id :initarg :node-id)
   (datum :accessor dfa-node-datum :initarg :datum)
   (arg-t :accessor dfa-node-arg-t :initarg :arg-t :initform nil)
   (des-ts :accessor dfa-node-des-ts :initarg :des-ts :initform nil)
   (dd-t :accessor dfa-node-dd-t :initarg :dd-t :initform nil)
   (accept-t :accessor dfa-node-accept-t :initarg :accept-t :initform nil)
   (opt-ts :accessor dfa-node-opt-ts :initarg :opt-ts :initform nil)))

(defmethod same-state ((obj1 dfa-node) (obj2 dfa-node))
  (and (subsetp (dfa-node-datum obj1) (dfa-node-datum obj2) :key #'nfa-node-id)
       (subsetp (dfa-node-datum obj2) (dfa-node-datum obj1) :key #'nfa-node-id)))

(defclass dfa ()
  ((root :accessor dfa-root :initarg :root)
   (lookup :accessor dfa-lookup :initarg :lookup
           :initform (make-array 5 :adjustable t :fill-pointer 0))))

(defvar *dfa*)
(defvar *dfa-computed-transitions*)

(defun nfa-to-dfa (nfa)
  (let ((*nfa* nfa)
        (*dfa* (make-instance 'dfa))
        (*dfa-computed-transitions* (make-hash-table))
        (root (make-instance 'dfa-node)))
    (setf (dfa-root *dfa*) root
          (dfa-node-datum root) (list (nfa-root *nfa*)))
    (intern-dfa-state root)
    (dfa-calculate-transitions (dfa-node-id root))
    *dfa*))

(defun dfa-lookup-state (state)
  (aref (dfa-lookup *dfa*) state))

(defun intern-dfa-state (node)
  (setf (dfa-node-datum node) (epsilon-closures (dfa-node-datum node)))
  (let ((old-state (iter (for id index-of-vector (dfa-lookup *dfa*))
                         (finding id such-that (same-state node (dfa-lookup-state id))))))
    (if old-state
        old-state
        (setf (dfa-node-id node) (vector-push-extend node (dfa-lookup *dfa*))))))

(defun dfa-node-all-transitions (node)
  (let ((transitions '()))
    (flet ((collecting (x) (push x transitions))
           (appending (x) (mapc (lambda (y) (push y transitions)) x)))
      (when (dfa-node-arg-t node) (collecting (dfa-node-arg-t node)))
      (when (dfa-node-accept-t node) (collecting (dfa-node-accept-t node)))
      (when (dfa-node-dd-t node) (collecting (dfa-node-dd-t node)))
      (appending (dfa-node-des-ts node))
      (appending (dfa-node-opt-ts node)))
    (nreverse transitions)))

(defun dfa-calculate-transitions (state)
  (unless (gethash state *dfa-computed-transitions*)
    (setf (gethash state *dfa-computed-transitions*) t)
    (let* ((node (dfa-lookup-state state))
           (arg-t '())
           (des-ts (make-hash-table :test 'equal))
           (dd-t '())
           (accept-t '())
           (opt-ts (make-hash-table :test 'equal)))
      (iter (for nfa-node in (dfa-node-datum node))
            (iter (for transition in (nfa-node-transitions nfa-node))
                  (let ((edge (transition-edge transition)))
                    (when edge
                      (etypecase edge
                        (arg-spec (setf arg-t (adjoin (transition-out transition)
                                                      arg-t)))
                        (des-spec (setf (gethash (des-string edge) des-ts)
                                        (adjoin (transition-out transition)
                                                (gethash (des-string edge) des-ts '()))))
                        (opt-spec (setf (gethash (opt-short edge) opt-ts)
                                        (adjoin (transition-out transition)
                                                (gethash (opt-short edge) opt-ts '()))))
                        (symbol
                         (ecase edge
                           (double-dash (setf dd-t (adjoin (transition-out transition)
                                                           dd-t)))
                           (accept (setf accept-t (adjoin (transition-out transition)
                                                          accept-t))))))))))
      ;; Generate our next states...
      (when arg-t
        (setf (dfa-node-arg-t node)
              (make-instance 'transition
                             :edge (make-instance 'arg-spec)
                             :out (intern-dfa-state (make-instance 'dfa-node :datum (map 'list #'lookup-state arg-t))))))
      (maphash (lambda (des states)
                 (push (make-instance 'transition
                                      :edge (make-instance 'des-spec :string des)
                                      :out (intern-dfa-state (make-instance 'dfa-node :datum (map 'list #'lookup-state states))))
                       (dfa-node-des-ts node)))
               des-ts)
      (when dd-t
        (setf (dfa-node-dd-t node)
              (make-instance 'transition
                             :edge 'double-dash
                             :out (intern-dfa-state (make-instance 'dfa-node :datum (map 'list #'lookup-state dd-t))))))
      (when accept-t
        (setf (dfa-node-accept-t node)
              (make-instance 'transition
                             :edge 'accept
                             :out (intern-dfa-state (make-instance 'dfa-node :datum (map 'list #'lookup-state accept-t))))))
      (maphash (lambda (opt states)
                 (push (make-instance 'transition
                                      :edge (make-instance 'opt-spec :short opt)
                                      :out (intern-dfa-state (make-instance 'dfa-node :datum (map 'list #'lookup-state states))))
                       (dfa-node-opt-ts node)))
               opt-ts)
      (iter (for transition in (dfa-node-all-transitions node))
            (dfa-calculate-transitions (transition-out transition))))))


;; Generating code.

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

(defun symb (&rest objs)
  (intern (string-upcase (with-output-to-string (s) (format s "~{~A~}" objs)))))

(defcode dfa-state-symbol (node)
  (symb "state" (if (numberp node) node (dfa-node-id node))))

(defcode arg-parse-driver (dfa)
  `(defun arg-parse-driver (tokens)
     (let ((state (function ,(generate-code 'dfa-state-symbol (dfa-root dfa)))))
       (dolist (token tokens)
         (setf state (funcall state token)))
       (funcall state nil))))

(defcode dfa-state-declaim (node)
  `(declaim (ftype function ,(generate-code 'dfa-state-symbol node))))

(defcode dfa-state (node)
  `(defun ,(generate-code 'dfa-state-symbol node) (token)
     (declare (ignorable token))
     (cond
       ,@(when (dfa-node-dd-t node)
           `(((string= "--" token)
              (function
               ,(generate-code 'dfa-state-symbol (transition-out (dfa-node-dd-t node)))))))
       ,@(when (dfa-node-accept-t node)
           `(((null token)
              ,(dfa-node-id node))))
       ,@(iter (for transition in (dfa-node-des-ts node))
               (collecting
                `((string= token ,(des-string (transition-edge transition)))
                  (function ,(generate-code 'dfa-state-symbol (transition-out transition))))))
       ,@(when (dfa-node-arg-t node)
           `(((and (stringp token) (not (short-opt-p token)) (not (long-opt-p token)))
              (function
               ,(generate-code 'dfa-state-symbol (transition-out (dfa-node-arg-t node)))))))
       ,@(iter (for transition in (dfa-node-opt-ts node))
               (collecting
                `((string= token ,(opt-short (transition-edge transition)))
                  (function ,(generate-code 'dfa-state-symbol (transition-out transition))))))
       (:otherwise (error "no transition from state ~A" ,(dfa-node-id node))))))


;; All the code we need in a given package.

(defun generate-parser-forms (dfa &optional (package *package*))
  (let ((*package* package)
        (noarg-code (list 'reintern-to-package

                          'arg-spec
                          'des-spec
                          'opt-spec
                          'arg-spec-load-form
                          'des-spec-load-form
                          'opt-spec-load-form

                          'identifier-char-p
                          'short-opt-p
                          'long-opt-p
                          'combined-short-opt-p
                          'single-opt-p
                          'opt-p
                          'included-arg-opt-p
                          'included-arg
                          'expand-short
                          'maybe-expand
                          'fully-expand-args
                          'normalize-args)))
    `(progn
       ,@(iter (for symbol in noarg-code)
               (collecting (generate-code symbol)))
       ,@(iter (for node in-vector (dfa-lookup dfa))
               (collecting (generate-code 'dfa-state-declaim node)))
       ,@(iter (for node in-vector (dfa-lookup dfa))
               (collecting (generate-code 'dfa-state node)))
       ,(generate-code 'arg-parse-driver dfa))))

(defun generate-copyable-parser (dfa)
  "The point of this parser is to be text that gets copied into another file --
   for example, a standalone script."
  ;; We do the form generation in one package, and the printing in another.
  ;; We don't want FORMAT to print symbols while it itself is in the parser's
  ;; package.
  (let ((forms (let ((*package* (make-package "COLANDER/PARSER" :use '(#:cl))))
                 (list `(defpackage #:colander/parser
                          (:use #:cl)
                          (:export #:parse))
                       (reintern-to-package
                        `(defvar old-package))
                       (reintern-to-package
                        `(eval-when (:compile-toplevel :load-toplevel :execute)
                           (setf old-package *package*)))
                       (reintern-to-package
                        `(in-package #:colander/parser))
                       (generate-parser-forms dfa)
                       (reintern-to-package
                        `(eval-when (:compile-toplevel :load-toplevel :execute)
                           (setf *package* old-package)))))))
    (prog1 (with-output-to-string (out)
             (let ((*package* (make-package "COLANDER/BLANK" :use '())))
               (format out "~{~S~^~%~}" forms)))
      (delete-package "COLANDER/PARSER")
      (delete-package "COLANDER/BLANK"))))
