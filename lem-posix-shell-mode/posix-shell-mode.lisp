#| link : http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html |#
(defpackage :lem-posix-shell-mode
  (:use :cl :lem :lem.language-mode)
  (:export :*posix-shell-mode-hook*))
(in-package :lem-posix-shell-mode)

(defvar *posix-shell-mode-hook* '())

(defparameter *reserved-words*
  '("case" "do" "done" "elif" "else"
    "esac" "fi" "for" "if" "in"
    "then" "until" "while"
    "function" "select"))

(defparameter *shell-variables*
  '("ENV" "HOME" "IFS" "LANG" "LC_ALL"
    "LC_COLLATE" "LC_CTYPE" "LC_MESSAGES" "LINENO" "NLSPATH"
    "PATH" "PPID" "PS1" "PS2" "PS4" "PWD"))

(defparameter *special-built-in-utilities*
  '("break" "colon" "continue" "dot" "eval"
    "exec" "exit" "export" "readonly" "return"
    "set" "shift" "times" "trap" "unset"))

(defun tokens (boundary strings)
  (let ((alternation
         `(:alternation ,@(sort (copy-list strings) #'> :key #'length))))
    (if boundary
        `(:sequence ,boundary ,alternation ,boundary)
        alternation)))

(defun make-tm-string-region (sepalator)
  (make-tm-region `(:sequence ,sepalator)
                  `(:sequence ,sepalator)
                  :name 'syntax-string-attribute
                  :patterns (make-tm-patterns (make-tm-match "\\\\."))))

(defun parameter-expression ()
  (let ((special-parameters "[*@#?\\-$!0]")
        (name "([a-zA-Z_][0-9a-zA-Z_]*)"))
    (format nil "\\$((~a)|(~a)|(\\{~a\\}))" special-parameters name name)))

(defun make-tmlanguage-posix-shell ()
  (let* ((patterns (make-tm-patterns
                    (make-tm-region "#" "$" :name 'syntax-comment-attribute)
                    (make-tm-string-region "\"")
                    (make-tm-string-region "'")
                    (make-tm-match (tokens :word-boundary
                                           *reserved-words*)
                                   :name 'syntax-keyword-attribute)
                    (make-tm-match (parameter-expression)
                                   :name 'syntax-variable-attribute)
                    (make-tm-match (tokens :word-boundary
                                           *shell-variables*)
                                   :name 'syntax-variable-attribute)
                    (make-tm-match (tokens :word-boundary
                                           *special-built-in-utilities*)
                                   :name 'syntax-builtin-attribute))))
    (make-tmlanguage :patterns patterns)))

(defvar *posix-shell-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :paren-pairs '((#\[ . #\])
                               (#\{ . #\}))
                :string-quote-chars '(#\" #\')
                :line-comment-string "#"))
        (tmlanguage (make-tmlanguage-posix-shell)))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode posix-shell-mode language-mode
    (:name "posix-shell"
     :keymap *posix-shell-mode-keymap*
     :syntax-table *posix-shell-syntax-table*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 4
        (variable-value 'calc-indent-function) 'posix-shell-calc-indent
        (variable-value 'line-comment) "#")
  (run-hooks *posix-shell-mode-hook*))

(defun posix-shell-calc-indent (point)
  (with-point ((point point))
    (let ((tab-width (variable-value 'tab-width :default point))
          (column (point-column point)))
      (+ column (- tab-width (rem column tab-width))))))

(pushnew (cons "\\.sh$" 'posix-shell-mode) *auto-mode-alist* :test #'equal)
(pushnew (cons "^.bashrc$" 'posix-shell-mode) *auto-mode-alist* :test #'equal)
(pushnew (cons "^.profile$" 'posix-shell-mode) *auto-mode-alist* :test #'equal)
