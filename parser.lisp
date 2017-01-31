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

(defcode dfa-state-symbol (dfa-node)
  (symb "state" (slot-value dfa-node 'id)))

(defcode arg-parse-driver (dfa)
  `(defun arg-parse-driver (tokens)
     (let ((state (function ,(generate-code 'dfa-state-symbol (dfa-root dfa)))))
       (dolist (token tokens)
         (setf state (funcall state token)))
       (funcall state nil))))

;; We add DECLAIMs to our generated parser so that SBCL doesn't whine about
;; undeclared functions in our mutual recursion.
(defcode dfa-state-declaim (dfa-node)
  `(declaim (ftype function ,(generate-code 'dfa-state-symbol dfa-node))))
