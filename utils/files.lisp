(in-package #:colander/utils)

(defun slurp (filename)
  "Read the contents of a file into a string."
  (with-open-file (input filename
                         :direction :input
                         :element-type 'character)
    (with-output-to-string (output)
      (iter (for char = (read-char input nil nil))
            (while char)
            (write-char char output)))))

(defun spit (string filename &optional (append? nil))
  "Print out the contents of the string into the file."
  (with-open-file (output filename
                          :direction :output
                          :element-type 'character
                          :if-does-not-exist :create
                          :if-exists (if append? :append :overwrite))
    (write-string string output)))
