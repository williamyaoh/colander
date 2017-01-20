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


(defun &symbolp (symb)
  (and (symbolp symb)
       (string= (symbol-name symb) "&" :end1 1)))

(defun designators (dlist)
  (remove-if #'&symbolp (flatten dlist)))

(defun merge-values% (values1 values2)
  (map 'list
       (lambda (value1 value2)
         `(,(first value2)
           ,(max (second value1) (second value2))
           ,(cons (third value2) (third value1))))
       values1
       values2))

(defun tee (obj)
  "For debugging."
  (format *error-output* "~S~%" obj)
  obj)

;; Structure of a value: (KEY NEST VALUE)
;;   where NEST indicates levels nesting within &REST, &BODY, &GROUP forms.
;;   NEST of 0 indicates singular value.
(defun destructure (dlist list &optional (nest 0))
  (iter
    (with state = 'positional)
    (with list* = list)
    (when (null list*) (finish))
    (let ((head (pop dlist)))
      (case head
        (&whole (setf state 'whole))
        ((&rest &body) (setf state 'rest))
        (&group (setf state 'group))
        (otherwise
         (ecase state
           (whole (collecting `(,head ,nest ,list))
            (setf state 'positional))
           (positional
            (let ((element (pop list*)))
              (if (listp element)
                  (appending (destructure head element nest))
                  (collecting `(,head ,nest ,element)))))
           (rest
            (if (not (listp head))
                (collecting `(,head ,(1+ nest) ,list*))
                (appending
                 (map 'list
                      (lambda (value)
                        `(,(first value)
                          ,(1+ (second value))
                          ,(nreverse (third value))))
                      (reduce #'merge-values%
                              (map 'list
                                   (lambda (l) (destructure head l nest))
                                   list*)
                              :initial-value
                              (map 'list
                                   (lambda (key) `(,key ,nest ()))
                                   (designators head))))))
            (finish))
           (group
            (appending (destructure `(&rest ,head) (group (length head) list*)))
            (finish))))))))

(defun maptree-nest (fn nest tree)
  (if (= nest 1)
      (mapcar fn tree)
      (mapcar (lambda (tree*) (mapcar-nest fn (1- nest) tree*)) tree)))

(defun mapcar-nest (fn nest tree) (flatten (maptree-nest fn nest tree)))

(defun verify-fn-schema (schema list)
  (every (lambda (fn-spec)
           (destructuring-bind (fn nest tree) fn-spec
             (if (zerop nest)
                 (funcall fn tree)
                 (iter
                   (for bool in (maptree-nest fn nest tree))
                   (always bool)))))
         (destructure schema list)))


(defclass arg-spec ()
  ((name :accessor arg-name :initarg :name)
   (many? :accessor arg-many? :initarg :many? :initform nil)))
(defclass designator-spec ()
  ((string :accessor designator-string :initarg :string)))
(defclass opt-spec ()
  ((short :accessor opt-short :initarg :short)
   (many? :accessor opt-many? :initarg :many? :initform nil)
   (arg? :accessor opt-arg? :initarg :arg? :initform nil)))

(defclass normal-parse-state ()
  ((items :accessor state-items :initarg :items)))
(defclass opt-arg-parse-state ()
  ((opt-name :accessor state-opt-name :initarg :opt-name)
   (has-arg-items :accessor state-has-arg-items :initarg :has-arg-items)
   (no-arg-items :accessor state-no-arg-items :initarg :no-arg-items)))

(defclass item ()
  ((production :accessor item-production :initarg :production)
   (dot :accessor item-dot :initarg :dot :initform 0)))

(defvar *productions*)

(defun get-production (prod-id)
  (aref *productions* prod-id))
(defun object-at-dot (item)
  (nth (item-dot item)
       (get-production (item-production item))))
(defun advance-dot (item)
  (make-instance 'item :dot (1+ (item-dot item)) :production (item-production item)))

(defgeneric same-state-p (obj1 obj2)
  (:documentation "Check if the two parse objects designate the same parse state.")
  (:method (obj1 obj2) nil))

(defun equal-closure (closure1 closure2)
  (and (subsetp closure1 closure2 :test #'same-state-p)
       (subsetp closure2 closure1 :test #'same-state-p)))

(defmethod same-state-p ((obj1 item) (obj2 item))
  (and (= (item-production obj1) (item-production obj2))
       (= (item-dot obj1) (item-dot obj2))))
(defmethod same-state-p ((obj1 normal-parse-state) (obj2 normal-parse-state))
  (equal-closure (state-items obj1) (state-items obj2)))
(defmethod same-state-p ((obj1 opt-arg-parse-state) (obj2 opt-arg-parse-state))
  (and (equal (state-opt-name obj1) (state-opt-name obj2))
       (equal-closure (state-has-arg-items obj1) (state-has-arg-items obj2))
       (equal-closure (state-no-arg-items obj1) (state-no-arg-items obj2))))

(defvar *seen-states*)

(defgeneric transitions (state)
  )

(defun intern-state (state)
  (or (find state *seen-states* :test #'same-state-p)
      (progn (push state *seen-states*)
             state)))

(defparameter *any-token-transition* (complement #'null))

(defmethod transitions ((state normal-parse-state))
  (let ((opts (make-hash-table :test 'equal))
        (opts-arg (make-hash-table :test 'equal))
        (terminated '())
        (args '())
        (designators (make-hash-table :test 'equal)))
    (iter
      (for item in (state-items state))
      (for next-element = (object-at-dot item))
      (etypecase next-element
        (null (setf terminated (adjoin item terminated)))
        (arg-spec (setf args (adjoin item args)))
        (designator-spec
         (setf (gethash (designator-string next-element) designators)
               (adjoin item (gethash (designator-string next-element)
                                     designators
                                     '()))))
        (list
         (iter
           (for opt-spec in next-element)
           (setf (gethash (opt-short opt-spec) opts)
                 (adjoin item (gethash (opt-short opt-spec) opts '())))
           (when (opt-arg? opt-spec)
             (setf (gethash (opt-short opt-spec) opts-arg)
                   (adjoin item (gethash (opt-short opt-spec) opts-arg '()))))))))
    (let `((,#'null                     ; $-transition
             accept-state
             ,(constantly 'accept))
           ;; designators
           ,@(maphash (lambda (key val)
                        `(,(lambda (token) (string= token key))
                          ,(intern-state (make-instance 'normal-parse-state
                                                        :items (map 'list #'advance-dot (union args val))))
                          ,(constantly 'no-value))))
           (,(complement #'opt-p)
             ,(intern-state (make-instance 'normal-parse-state :items (map 'list #'advance-dot args)))
            ,(lambda (token) `(:operand )))
           ))))

(defmethod transitions ((state opt-arg-parse-state))
  (iter
    (for (test-fn items value-fn)
         in (list
             `(,(complement
                 (one-of #'opt-p (partial #'string= "--")))
               ,(state-has-arg-items state)
               ,(lambda (token) `(:operand ,(state-opt-name state) ,token)))
             `(,#'opt-p
               ,(state-no-arg-items state)
               ,(constantly 'no-shift))))
    (when items
      (collecting `(,test-fn
                    ,(intern-state
                      (make-instance 'normal-parse-state :items items))
                    ,value-fn)))))

(defvar *cached-driver-functions*)

(defgeneric state-driver-function (state)
  (:documentation
   "Return a function of one argument (a token), and multiple-return a) a
    function representing the next state and b) a parsed value."))

(defmethod state-driver-function :around (state)
  (let ((fn (cdr (assoc state *cached-driver-functions* :test #'same-state-p))))
    (if (null fn)
        (let ((new-fn (call-next-method)))
          (push (cons state new-fn) *cached-driver-functions*)
          new-fn)
        fn)))

(defmethod state-driver-function ((state normal-parse-state))
  ;; TODO: write this
  'dummy)

(defmacro or* (&rest forms)
  "OR, but patched so the first form that returns true also returns its
   secondary values."
  (let ((block-name (gensym "BLOCK"))
        (val-name (gensym "VAL")))
    `(block ,block-name
       ,@(map 'list
              (lambda (form)
                `(let ((,val-name (multiple-value-list ,form)))
                   (when (car ,val-name)
                     (return-from ,block-name (apply #'values ,val-name)))))
              forms))))

(defmethod state-driver-function ((state opt-arg-parse-state))
  (let ((transition-fns (map 'list
                             (lambda (transition)
                               `(,(first transition)
                                 ,(state-driver-function (second transition))
                                 ,(third transition)))
                             (transitions state))))
    (lambda (token)
      (or* (iter
             (for (test-fn state-fn value-fn) in transition-fns)
             (when (funcall test-fn token)
               (leave (values state-fn (funcall value-fn token)))))
           (when (string= token "--" :end1 2)
             (setf *double-dash* t)
             ;; TODO: Error code for unallowed double-dash in opt argument here
             )
           ;; TODO: Error code when everything fell through
           ))))

(defvar *double-dash*)

;; (defun drive-opt-parser (driver args)
;;   (iter
;;     (for str in args)
;;     (multiple-value-bind (driver* value) (funcall driver str)
;;       (typecase value
;;         ((parsed-arg parsed-opt)
;;          (collecting `(,(operand-name value) ,(operand-value value)))))
;;       (setf driver driver*)
;;       (until (eq driver* 'accept)))))

(let ((*productions* (make-array 5 :adjustable t))
      (*seen-states '()))
  )
