;;; int-mode.el --- A major mode for editing Intercept source code -*- lexical-binding: t -*-

;;; Code:

(defvar int-mode-syntax-table
  (make-syntax-table)
  "Syntax table for int-mode")

;; Set two semi-colons as comment starting characters.
(modify-syntax-entry
 ?\; ". 12"
 int-mode-syntax-table)
;; Set newline as comment ending character.
(modify-syntax-entry
 ?\n ">"
 int-mode-syntax-table)

;; Delimiters include comma
(modify-syntax-entry
 ?, "."
 int-mode-syntax-table)

;; Allowed within words: \_
(modify-syntax-entry
 ?_ "w"
 int-mode-syntax-table)
(modify-syntax-entry
 ?\\ "w"
 int-mode-syntax-table)

;; Parenthesis
(modify-syntax-entry
 ?\( "()"
 int-mode-syntax-table)
(modify-syntax-entry
 ?\) ")("
 int-mode-syntax-table)
(modify-syntax-entry
 ?\[ "(]"
 int-mode-syntax-table)
(modify-syntax-entry
 ?\] ")["
 int-mode-syntax-table)
(modify-syntax-entry
 ?{ "(}"
 int-mode-syntax-table)
(modify-syntax-entry
 ?} "){"
 int-mode-syntax-table)

;; Unary prefix operators
;; Addressof
(modify-syntax-entry
 ?& "'"
 int-mode-syntax-table)
;; Dereference
(modify-syntax-entry
 ?@ "'"
 int-mode-syntax-table)

(defcustom int-mode-face-keywords 'font-lock-keyword-face
  "Symbol name of face to highlight int-mode keywords with."
  :group 'int-mode
  :tag "Syntax Highlight Keywords")

(defcustom int-mode-face-types 'font-lock-type-face
  "Symbol name of face to highlight int-mode builtin types with."
  :group 'int-mode
  :tag "Syntax Highlight Types")

(defcustom int-mode-face-numbers 'font-lock-number-face
  "Symbol name of face to highlight int-mode immediate numbers with."
  :group 'int-mode
  :tag "Syntax Highlight Numbers")

(defcustom int-mode-face-strings 'font-lock-string-face
  "Symbol name of face to highlight int-mode immediate numbers with."
  :group 'int-mode
  :tag "Syntax Highlight Strings")

(defcustom int-mode-face-operators 'font-lock-operator-face
  "Symbol name of face to highlight int-mode builtin binary operators with.
Examples include addition (+) and subtraction (-)."
  :group 'int-mode
  :tag "Syntax Highlight Binary Operators")

(defcustom int-mode-face-negation-char 'font-lock-negation-char-face
  "Symbol name of face to highlight int-mode negation character (!) with."
  :group 'int-mode
  :tag "Syntax Highlight Negation Character")

(defcustom int-mode-face-macro-args 'font-lock-property-face
  "Symbol name of face to highlight int-mode macro arguments with (starts with $)."
  :group 'int-mode
  :tag "Syntax Highlight Macro Arguments")

(defcustom int-mode-face-function-names 'font-lock-function-name-face
  "Symbol name of face to highlight int-mode function names in definitions."
  :group 'int-mode
  :tag "Syntax Highlight Function Names")

;; Gather all keyword font locks together into big daddy keyword font-lock
(setq int--font-lock-defaults
      (let* ((keywords '("if" "else" "while" "for" "type" "ext" "macro" "emits" "endmacro" ;; keywords
                         "discardable ""nomangle" ;; function attributes
                         "alignas" ;; type attributes
                         ))
             (operators '("+" "*" "-" "/" "%"
                          "<" ">"
                          ":" ";" "=" ":=" "::"
                          "&" "@"
                          ">>" "<<" "&" "|" "~"))

             (macro-arg-regex        "\\$[a-zA-Z_][a-zA-Z0-9_]*")
             (keywords-regex         (regexp-opt keywords 'words))
             (operators-regex        (regexp-opt operators))
             (negation-char-regex    (regexp-opt '("!" "~")))
             (number-regex           (rx (one-or-more digit)))
             (string-regex           (rx "\"" (*? (not "\"")) "\""))
             (builtin-types-regex    (rx (zero-or-more "&")
                                         (zero-or-more "@")
                                         (or "integer" "byte" "void"))))
        `(
          (,macro-arg-regex         . ',int-mode-face-macro-args)
          (,keywords-regex          . ',int-mode-face-keywords)
          (,builtin-types-regex     . ',int-mode-face-types)
          (,number-regex            . ',int-mode-face-numbers)
          (,operators-regex         . ',int-mode-face-operators)
          (,negation-char-regex     . ',int-mode-face-negation-char)
          (,string-regex            . ',int-mode-face-strings)
          )))

(defcustom int-mode-indent-amount 2
  "The amount of space characters that each level of parenthesis nesting
in Intercept source code will be indented."
  :group 'int-mode)

(defun int--indent-line ()
  "Indent a line in Intercept."
  (let ((indent)
        (boi-predicate)
        (should-move-eol)
        (point (point)))
    (save-excursion
      ;; Move 'point' backwards to end of indentation, if any.
      (back-to-indentation)
      ;; Use the partial parsing syntax state or whatever to get
      ;; parenthesis nesting level. This value is calculated using
      ;; data from the syntax table up above.
      (setq indent (car (syntax-ppss))
            boi-predicate (= (point) point))
      (if boi-predicate
          ;; If we began at the beginning of the line (after indentation),
          ;; we need to move point to after inserted indentation.
          (setq should-move-eol t)
        ;; If not at beginning of indentation but at a newline, do not
        ;; indent anything at all. FIXME: Is this misleading?
        (when (eq (char-after) ?\n)
          (setq indent 0)))
      ;; Closing parenthesis characters are indented at parent level.
      (when (or
             (eq (char-after) ?\))
             (eq (char-after) ?\}))
        (setq indent (1- indent)))
      ;; Get rid of existing indentation, if any.
      (delete-region (line-beginning-position) (point))
      ;; Indent to proper amount based on customizable value.
      (indent-to (* indent int-mode-indent-amount)))
    (when should-move-eol
      (move-end-of-line nil))))

(define-derived-mode int-mode prog-mode
  "Intercept"
  (setq-local comment-start ";;")
  (setq-local comment-end   "")
  (setq font-lock-defaults '((int--font-lock-defaults)))
  (setq indent-line-function #'int--indent-line))

(provide 'int-mode)

;;; int-mode.el ends here
