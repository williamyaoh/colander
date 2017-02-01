# COLANDER

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

Copyright (c) 2017 William Yao

## Goals

+ Generate parsing code from declarative specifications of CLI
+ Generated code should have no dependencies other than CL itself
  * In other words, parsers are completely standalone; they don't even need this library
+ Handle a wide range of syntaxes in use
  * long vs. short options
  * double dash `--`,
  * specifying an option's argument separately `-b 20` vs. together `-b=20`
  * collapsed short options `-iavh`
* Be convenient to use
* Write beautiful code

## Description and usage

Currently in a very preliminary state, but it works.

```lisp
CL-USER> (defparameter parser
           (colander:cli-specs-to-parser%
            '("add" (:opts "-iavh") (:arg "-b" "BOUND") :filename)
            '("init" (:opt "-h") (:opt "-v"))))
PARSER

CL-USER> (colander:output-parser-to-file% parser "cli-parser.lisp")
```

```lisp
;; <in "cli-parser.lisp">

(defun main (argv)
  (format t "~A~%" (colander-parser:parse argv)))
```
