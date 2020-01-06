;;
;; Copyright 2017 - 2020, MaiHD
;;

(require 'lisp-mode)

(defvar urn-mode-syntax-table
  (let ((st (make-syntax-table))
        (i 0))
    ;; Symbol constituents
    ;; We used to treat chars 128-256 as symbol-constituent, but they
    ;; should be valid word constituents (Bug#8843).  Note that valid
    ;; identifier characters are Scheme-implementation dependent.
    (while (< i ?0)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))

    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{ "(}  " st)
    (modify-syntax-entry ?} "){  " st)
    (modify-syntax-entry ?\| "\" 23bn" st)
    ;; Guile allows #! ... !# comments.
    ;; But SRFI-22 defines the comment as #!...\n instead.
    ;; Also Guile says that the !# should be on a line of its own.
    ;; It's too difficult to get it right, for too little benefit.
    ;; (modify-syntax-entry ?! "_ 2" st)

    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    ;; It's used for single-line comments as well as for #;(...) sexp-comments.
    (modify-syntax-entry ?\; "<"    st)
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?' "'   " st)
    (modify-syntax-entry ?` "'   " st)

    ;; Special characters
    (modify-syntax-entry ?, "'   " st)
    (modify-syntax-entry ?@ "'   " st)
    (modify-syntax-entry ?# "' 14" st)
    (modify-syntax-entry ?\\ "\\   " st)
    st))

(defvar urn-mode-abbrev-table nil)
(define-abbrev-table 'urn-mode-abbrev-table ())

(defvar urn-imenu-generic-expression
      '((nil
         "^(\\(def\\|define\\)\\(\\|-\\(generic\\(\\|-procedure\\)\\|method\\)\\)*\\s-+(?\\(\\sw+\\)" 4)
        ("Types"
         "^(define-class\\s-+(?\\(\\sw+\\)" 1)
        ("Macros"
         "^(\\(defmacro\\|define-macro\\|define-syntax\\)\\s-+(?\\(\\sw+\\)" 2))
  "Imenu generic expression for Urn mode.  See `imenu-generic-expression'.")

(defun urn-mode-variables ()
  (set-syntax-table urn-mode-syntax-table)
  (setq local-abbrev-table urn-mode-abbrev-table)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (setq-local adaptive-fill-mode nil)
  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local outline-regexp ";;; \\|(....")
  (setq-local add-log-current-defun-function #'lisp-current-defun-name)
  (setq-local comment-start ";")
  (setq-local comment-add 1)
  (setq-local comment-start-skip ";+[ \t]*")
  (setq-local comment-use-syntax t)
  (setq-local comment-column 40)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local lisp-indent-function 'urn-indent-function)
  (setq mode-line-process '("" urn-mode-line-process))
  (setq-local imenu-case-fold-search t)
  (setq-local imenu-generic-expression urn-imenu-generic-expression)
  (setq-local imenu-syntax-alist '(("+-*/.<>=?!$%_&~^:" . "w")))
  (setq-local syntax-propertize-function #'urn-syntax-propertize)
  (setq font-lock-defaults
        '((urn-font-lock-keywords
           urn-font-lock-keywords-1 urn-font-lock-keywords-2)
          nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
          beginning-of-defun
          (font-lock-mark-block-function . mark-defun)))
  (setq-local prettify-symbols-alist lisp-prettify-symbols-alist)
  (setq-local lisp-doc-string-elt-property 'urn-doc-string-elt))

(defvar urn-mode-line-process "")

(defvar urn-mode-map
  (let ((smap (make-sparse-keymap))
        (map (make-sparse-keymap "Urn")))
    (set-keymap-parent smap lisp-mode-shared-map)
    (define-key smap [menu-bar urn] (cons "Urn" map))
    (define-key map [run-urn] '("Run Inferior Urn" . run-urn))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)
    smap)
  "Keymap for Urn mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

;; Used by Urn
(defun urn-mode-commands (map)
  ;;(define-key map "\t" 'indent-for-tab-command) ; default
  (define-key map "\177" 'backward-delete-char-untabify)
  (define-key map "\e\C-q" 'indent-sexp))

;;;###autoload
(define-derived-mode urn-mode prog-mode "Urn"
  "Major mode for editing Urn code.
Editing commands are similar to those of `lisp-mode'.

In addition, if an inferior Urn process is running, some additional
commands will be defined, for evaluating expressions and controlling
the interpreter, and the state of the process will be displayed in the
mode line of all Urn buffers. Use \\[run-urn] to
start an inferior Urn.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{urn-mode-map}"
  (urn-mode-variables))

(defgroup urn nil
  "Editing Urn code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'lisp)

(defcustom urn-mode-hook nil
  "Normal hook run when entering `urn-mode'.
See `run-hooks'."
  :type 'hook
  :group 'urn)

(defcustom urn-program-name "urn"
  "Program invoked by the `run-urn' command."
  :type 'string
  :group 'urn)

(defconst urn-font-lock-keywords-1
  (eval-when-compile
    (list
     ;;
     ;; Declarations.  Hannes Haug <hannes.haug@student.uni-tuebingen.de> says
     ;; this works for SOS, STklos, SCOOPS, Meroon and Tiny CLOS.
     (list (concat "(\\(\\(def\\|define\\)\\*?\\("
                   ;; Function names.
                   "\\(\\|un\\|event\\|-public\\|-method\\|-generic\\(-procedure\\)?\\)\\|"
                   ;; Macro names, as variable names.  A bit dubious, this.
                   "\\(-syntax\\|-macro\\)\\|"
                   ;; Class names.
                   "-class"
                   ;; Guile modules.
                   "\\|-module"
                   "\\)\\)\\>"
                   ;; Any whitespace and declared object.
                   ;; The "(*" is for curried definitions, e.g.,
                   ;;  (define ((sum a) b) (+ a b))
                   "[ \t]*(*"
                   "\\(\\sw+\\)?")
           '(1 font-lock-keyword-face)
           '(6 (cond ((match-beginning 3) font-lock-function-name-face)
                     ((match-beginning 5) font-lock-variable-name-face)
                     (t font-lock-type-face))
               nil t))
     ))
  "Subdued expressions to highlight in Urn modes.")

(defconst urn-font-lock-keywords-2
  (append urn-font-lock-keywords-1
   (eval-when-compile
     (list
      ;;
      ;; Control structures.
      (cons
       (concat
        "(" (regexp-opt
             '("begin" "call-with-current-continuation" "call/cc"
               "call-with-input-file" "call-with-output-file" "case" "cond"
               "do" "else" "for-each" "if" "lambda" "λ"
               "let" "let*" "let-syntax" "letrec" "letrec-syntax"
               ;; R6RS library subforms.
               "export" "import"
               ;; SRFI 11 usage comes up often enough.
               "let-values" "let*-values"
               ;; Hannes Haug <hannes.haug@student.uni-tuebingen.de> wants:
               "and" "or" "delay" "force"
               ;; Stefan Monnier <stefan.monnier@epfl.ch> says don't bother:
               ;;"quasiquote" "quote" "unquote" "unquote-splicing"
	       "map" "syntax" "syntax-rules"
	       ;; For R7RS
	       "when" "unless" "letrec*" "include" "include-ci" "cond-expand"
	       "delay-force" "parameterize" "guard" "case-lambda"
	       "syntax-error" "only" "except" "prefix" "rename" "define-values"
	       "define-record-type" "define-library"
	       "include-library-declarations"
	       ;; SRFI-8
	       "receive"
	       ) t)
        "\\>") 1)
      ;;
      ;; It wouldn't be Urn w/o named-let.
      '("(let\\s-+\\(\\sw+\\)"
        (1 font-lock-function-name-face))
      ;;
      ;; David Fox <fox@graphics.cs.nyu.edu> for SOS/STklos class specifiers.
      '("\\<<\\sw+>\\>" . font-lock-type-face)
      ;;
      ;; Urn `:' and `#:' keywords as builtins.
      '("\\<#?:\\sw+\\>" . font-lock-builtin-face)
      ;; R6RS library declarations.
      '("(\\(\\<library\\>\\)\\s-*(?\\(\\sw+\\)?"
        (1 font-lock-keyword-face)
        (2 font-lock-type-face))
      )))
  "Gaudy expressions to highlight in Urn modes.")

(defvar urn-font-lock-keywords urn-font-lock-keywords-1
  "Default expressions to highlight in Urn modes.")

(defconst urn-sexp-comment-syntax-table
  (let ((st (make-syntax-table urn-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?#  "'" st)
    st))

(put 'lambda 'urn-doc-string-elt 2)
;; Docstring's pos in a `define' depends on whether it's a var or fun def.
(put 'define 'urn-doc-string-elt
     (lambda ()
       ;; The function is called with point right after "define".
       (forward-comment (point-max))
       (if (eq (char-after) ?\() 2 0)))

(defun urn-syntax-propertize (beg end)
  (goto-char beg)
  (urn-syntax-propertize-sexp-comment (point) end)
  (funcall
   (syntax-propertize-rules
    ("\\(#\\);" (1 (prog1 "< cn"
                     (urn-syntax-propertize-sexp-comment (point) end)))))
   (point) end))

(defun urn-syntax-propertize-sexp-comment (_ end)
  (let ((state (syntax-ppss)))
    (when (eq 2 (nth 7 state))
      ;; It's a sexp-comment.  Tell parse-partial-sexp where it ends.
      (condition-case nil
          (progn
            (goto-char (+ 2 (nth 8 state)))
            ;; FIXME: this doesn't handle the case where the sexp
            ;; itself contains a #; comment.
            (forward-sexp 1)
            (put-text-property (1- (point)) (point)
                               'syntax-table (string-to-syntax "> cn")))
        (scan-error (goto-char end))))))

(defvar calculate-lisp-indent-last-sexp)


;; FIXME this duplicates almost all of lisp-indent-function.
;; Extract common code to a subroutine.
(defun urn-indent-function (indent-point state)
  "Urn mode function for the value of the variable `lisp-indent-function'.
This behaves like the function `lisp-indent-function', except that:

i) it checks for a non-nil value of the property `urn-indent-function'
\(or the deprecated `urn-indent-hook'), rather than `lisp-indent-function'.

ii) if that property specifies a function, it is called with three
arguments (not two), the third argument being the default (i.e., current)
indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (get (intern-soft function) 'urn-indent-function)
                         (get (intern-soft function) 'urn-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
                (funcall method state indent-point normal-indent)))))))


;;; Let is different in Urn

;; (defun urn-would-be-symbol (string)
;;   (not (string-equal (substring string 0 1) "(")))

;; (defun run-next-sexp-as-string ()
;;   ;; Assumes that it is protected by a save-excursion
;;   (forward-sexp 1)
;;   (let ((the-end (point)))
;;     (backward-sexp 1)
;;     (buffer-substring (point) the-end)))

;; This is correct but too slow.
;; The one below works almost always.
;;(defun urn-let-indent (state indent-point)
;;  (if (urn-would-be-symbol (urn-next-sexp-as-string))
;;      (urn-indent-specform 2 state indent-point)
;;      (urn-indent-specform 1 state indent-point)))

(defun urn-let-indent (state indent-point normal-indent)
  (skip-chars-forward " \t")
  (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
      (lisp-indent-specform 2 state indent-point normal-indent)
    (lisp-indent-specform 1 state indent-point normal-indent)))

;; (put 'begin 'urn-indent-function 0), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'begin 'urn-indent-function 0)
(put 'case 'urn-indent-function 1)
(put 'delay 'urn-indent-function 0)
(put 'do 'urn-indent-function 2)
(put 'lambda 'urn-indent-function 1)
(put 'let 'urn-indent-function 'urn-let-indent)
(put 'let* 'urn-indent-function 1)
(put 'letrec 'urn-indent-function 1)
(put 'let-values 'urn-indent-function 1) ; SRFI 11
(put 'let*-values 'urn-indent-function 1) ; SRFI 11
(put 'sequence 'urn-indent-function 0) ; SICP, not r4rs
(put 'let-syntax 'urn-indent-function 1)
(put 'letrec-syntax 'urn-indent-function 1)
(put 'syntax-rules 'urn-indent-function 1)
(put 'syntax-case 'urn-indent-function 2) ; not r5rs
(put 'library 'urn-indent-function 1) ; R6RS

(put 'call-with-input-file 'urn-indent-function 1)
(put 'with-input-from-file 'urn-indent-function 1)
(put 'with-input-from-port 'urn-indent-function 1)
(put 'call-with-output-file 'urn-indent-function 1)
(put 'with-output-to-file 'urn-indent-function 1)
(put 'with-output-to-port 'urn-indent-function 1)
(put 'call-with-values 'urn-indent-function 1) ; r5rs?
(put 'dynamic-wind 'urn-indent-function 3) ; r5rs?

;; R7RS
(put 'when 'urn-indent-function 1)
(put 'unless 'urn-indent-function 1)
(put 'letrec* 'urn-indent-function 1)
(put 'parameterize 'urn-indent-function 1)
(put 'define-values 'urn-indent-function 1)
(put 'define-record-type 'urn-indent-function 1) ;; is 1 correct?
(put 'define-library 'urn-indent-function 1)

;; SRFI-8
(put 'receive 'urn-indent-function 2)

(provide 'urn-mode)

;;; urn.el ends here
