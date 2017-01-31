(in-package #:colander/code-generation)

(eval-now
  (defgeneric generate-code (code-name &rest args)
    (:documentation
     "Provide a way of generating code _into a different package_.
      Core of our parser generator; allows us to emit code without dependencies
      to parse command line options.")))

;; We use REINTERN-TO-PACKAGE within the definition of DEFCODE below,
;; but we also need it in our output code.
(eval-now
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
    (reintern-to-package (call-next-method))))

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
   and for generating somewhere else. BODY must be evaluable at compile time."
  `(eval-now
     (defcode ,name () ,@body)
     (eval (generate-code ',name))))

(defmacro defcodefn! (name (&rest args) &body body)
  "Used for defining functions which are useful both in the current package
   and for generating somewhere else."
  `(defcode! ,name '(defun ,name ,args ,@body)))
