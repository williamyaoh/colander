(in-package #:colander)

;; Firstly, a standard syntax for our specifications.

(defclass arg-spec ()
  ((name :accessor arg-name :initarg :name)
   (many? :accessor arg-many? :initarg :many? :initform nil)))
(defclass des-spec ()
  ((string :accessor des-string :initarg :string)))
(defclass opt-spec ()
  ((name :accessor opt-name :initarg :name)
   (short :accessor opt-short :initarg :short)
   (arg? :accessor opt-arg? :initarg :arg? :initform nil)
   (arg-name :accessor opt-arg-name :initarg :arg-name :initform nil)
   (many? :accessor opt-many? :initarg :many? :initform nil)))

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


;; NFA, and NFA-to-DFA translations.

(defvar *nfa*)
(defvar *nfa-computed-transitions*)

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
  (let ((old-state (iter (for id index-of-vector (dfa-lookup *dfa*))
                         (finding id such-that (same-state node (dfa-lookup-state id))))))
    (if old-state
        old-state
        (setf (dfa-node-id node) (vector-push-extend node (dfa-lookup *dfa*))))))

(defun dfa-node-all-transitions (node)
  (list* (dfa-node-arg-t node)
         (dfa-node-dd-t node)
         (dfa-node-accept-t node)
         (append (dfa-node-des-ts node)
                 (dfa-node-opt-ts node))))

(defun dfa-calculate-transitions (state)
  (unless (gethash state *dfa-computed-transitions*)
    (setf (gethash state *dfa-computed-transitions*) t)
    (let* ((node (dfa-lookup-state state))
           (closure (epsilon-closures (dfa-node-datum node)))
           (arg-t '())
           (des-ts (make-hash-table :test 'equal))
           (dd-t '())
           (accept-t '())
           (opt-ts (make-hash-table :test 'equal)))
      (setf (dfa-node-datum node) closure)
      (iter (for nfa-node in closure)
            (iter (for transition in (nfa-node-transitions nfa-node))
                  (let ((edge (transition-edge transition)))
                    (etypecase edge
                      (arg-spec (setf arg-t (adjoin (transition-out transition)
                                                    arg-t)))
                      (des-spec (setf (gethash des-ts (des-string edge))
                                      (adjoin (transition-out transition)
                                              (gethash des-ts (des-string edge) '()))))
                      (opt-spec (setf (gethash opt-ts (opt-short edge))
                                      (adjoin (transition-out transition)
                                              (gethash opt-ts (opt-short edge) '()))))
                      (symbol
                       (ecase edge
                         (double-dash (setf dd-t (adjoin (transition-out transition)
                                                         dd-t)))
                         (accept (setf accept-t (adjoin (transition-out transition)
                                                        accept-t)))))))))
      ;; Generate our next states...
      (setf (dfa-node-arg-t node)
            (make-instance 'transition
                           :edge (make-instance 'arg-spec)
                           :out (intern-dfa-state (make-instance 'dfa-node :datum (map 'list #'lookup-state arg-t)))))
      (maphash (lambda (des states)
                 (push (make-instance 'transition
                                      :edge (make-instance 'des-spec :string des)
                                      :out (intern-dfa-state (make-instance 'dfa-node :datum (map 'list #'lookup-state states))))
                       (dfa-node-des-ts node)))
               des-ts)
      (setf (dfa-node-dd-t node)
            (make-instance 'transition
                           :edge 'double-dash
                           :out (intern-dfa-state (make-instance 'dfa-node :datum (map 'list #'lookup-state dd-t)))))
      (setf (dfa-node-accept-t node)
            (make-instance 'transition
                           :edge 'accept
                           :out (intern-dfa-state (make-instance 'dfa-node :datum (map 'list #'lookup-state accept-t)))))
      (maphash (lambda (opt states)
                 (push (make-instance 'transition
                                      :edge (make-instance 'opt-spec :short opt)
                                      :out (intern-dfa-state (make-instance 'dfa-node :datum (map 'list #'lookup-state states))))
                       (dfa-node-opt-ts node)))
               opt-ts)
      (iter (for transition in (dfa-node-all-transitions node))
            (dfa-calculate-transitions (transition-out transition))))))


;; Generating code.

(defun symb (&rest objs)
  (intern (string-upcase (with-output-to-string (s)
                           (format s "~{~A~}" objs)))))

(defun state-code (node &optional (package *package*))
  "Generate the code for a single DFA state's parsing function. PACKAGE
   determines in which package the symbols in the generated code will come
   from."
  (let* ((*package* package)
         (token (symb "token"))
         (state-name (symb "state" (dfa-node-id node))))
    `(defun ,state-name (,token)
       (cond
         ,@(when (dfa-node-accept-t node)
             `(((null ,token) ,(transition-out (dfa-node-accept-t node)))))
         ,@(when (dfa-node-dd-t node)
             `(((string= ,token "--") ,(transition-out (dfa-node-dd-t node)))))
         ;; Put stuff here zzz...
         ))))


;; Actual strings.

(defmacro alias (name fn)
  "Set the SYMBOL-FUNCTION of NAME correctly. FN should be a form
   which evaluates to a function."
  `(progn
     (declaim (ftype function ,name))
     (setf (symbol-function ',name) ,fn)
     ',name))

(alias alpha-number-p #'alphanumericp)
(alias identifier-char-p (one-of #'alpha-number-p (lambda (c) (find c "-_"))))

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

(defun included-arg-opt-p (str)
  (when (stringp str)
    (let ((equal (position #\= str)))
      (when equal
        (and (not (zerop (length (subseq str (1+ equal)))))
             (opt-p (subseq str 0 equal)))))))

(defun included-arg (str)
  "Extract the included argument in the option; return them if found, NIL
   if not."
  (when (included-arg-opt-p str)
    (let ((equal (position #\= str)))
      (if (funcall #'opt-p (subseq str 0 equal))
          (values (subseq str (1+ equal))
                  (subseq str 0 equal))
          (values nil nil)))))

(defun expand-short (opt)
  (iter (for char in-vector (subseq opt 1))
        (collecting (format nil "-~C" char))))

(defun maybe-expand (opt)
  (if (long-opt-p opt)
      (list opt)
      (expand-short opt)))

(defun fully-expand-args (arg)
  "Return a list containing the embedded args (for example, '-abc=blah' gets
   expanded into (-a -b -c blah))"
  (multiple-value-bind (arg* opt*) (included-arg arg)
    (if arg*
        (append (maybe-expand opt*) (list arg*))
        (maybe-expand arg))))

(defun normalize-args (args)
  (iter (for arg in args)
        (with double-dash = nil)
        (if (not double-dash)
            (cond
              ((string= arg "--")
               (setf double-dash t)
               (collecting arg))
              ((not (funcall (one-of #'opt-p #'included-arg-opt-p) arg))
               (collecting arg))
              (:otherwise (appending (fully-expand-args arg))))
            (collecting arg))))
