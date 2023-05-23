;;; int-isel-mode.el --- A major mode for editing intc instruction selection tables -*- lexical-binding: t -*-

;;; Code:

(defvar int-isel-mode-syntax-table
  (make-syntax-table)
  "Syntax table for int-isel-mode")

;; Set two semi-colons as comment starting characters.
(modify-syntax-entry
 ?\; ". 12"
 int-isel-mode-syntax-table)
;; Set newline as comment ending character.
(modify-syntax-entry
 ?\n ">"
 int-isel-mode-syntax-table)

;; Delimiters include comma
(modify-syntax-entry
 ?, "."
 int-isel-mode-syntax-table)

;; Allowed within symbols: _-#$
(modify-syntax-entry
 ?_ "_"
 int-isel-mode-syntax-table)
(modify-syntax-entry
 ?- "_"
 int-isel-mode-syntax-table)
(modify-syntax-entry
 ?# "_"
 int-isel-mode-syntax-table)
(modify-syntax-entry
 ?$ "_"
 int-isel-mode-syntax-table)

;; Parenthesis
(modify-syntax-entry
 ?\( "()"
 int-isel-mode-syntax-table)
(modify-syntax-entry
 ?\) ")("
 int-isel-mode-syntax-table)
(modify-syntax-entry
 ?\[ "(]"
 int-isel-mode-syntax-table)
(modify-syntax-entry
 ?\] ")["
 int-isel-mode-syntax-table)
(modify-syntax-entry
 ?{ "(}"
 int-isel-mode-syntax-table)
(modify-syntax-entry
 ?} "){"
 int-isel-mode-syntax-table)

;; Unary prefix operators
;; Addressof
(modify-syntax-entry
 ?& "'"
 int-isel-mode-syntax-table)
;; Dereference
(modify-syntax-entry
 ?@ "'"
 int-isel-mode-syntax-table)

(defcustom int-isel-mode-face-keywords 'font-lock-keyword-face
  "Symbol name of face to highlight int-isel-mode keywords with."
  :group 'int-isel-mode
  :tag "Syntax Highlight Keywords")

(defcustom int-isel-mode-face-types 'font-lock-type-face
  "Symbol name of face to highlight int-isel-mode builtin types with."
  :group 'int-isel-mode
  :tag "Syntax Highlight Types")

(defcustom int-isel-mode-face-numbers nil
  "Symbol name of face to highlight int-isel-mode immediate numbers with."
  :group 'int-isel-mode
  :tag "Syntax Highlight Numbers")

(defcustom int-isel-mode-face-strings 'font-lock-string-face
  "Symbol name of face to highlight int-isel-mode immediate numbers with."
  :group 'int-isel-mode
  :tag "Syntax Highlight Strings")

(defcustom int-isel-mode-face-operators nil
  "Symbol name of face to highlight int-isel-mode builtin binary operators with.
Examples include addition (+) and subtraction (-)."
  :group 'int-isel-mode
  :tag "Syntax Highlight Binary Operators")

(defcustom int-isel-mode-face-negation-char 'font-lock-negation-char-face
  "Symbol name of face to highlight int-isel-mode negation character (!) with."
  :group 'int-isel-mode
  :tag "Syntax Highlight Negation Character")

(defcustom int-isel-mode-face-function-names 'font-lock-function-name-face
  "Symbol name of face to highlight int-isel-mode function names in definitions."
  :group 'int-isel-mode
  :tag "Syntax Highlight Function Names")

;; Gather all keyword font locks together into big daddy keyword font-lock
(setq int-isel--font-lock-defaults
      (let* ((keywords '("match" "emit" "discard"))
             (operators '("+" "*" "-" "/" "%"
                          "<" ">"
                          ":" ";" "=" ":=" "::"
                          "&" "@"
                          ">>" "<<" "&" "|" "~"))
             (builtin-types '("Immediate" "IMM" "Register" "REG"
                              "Name" "Block" "Function"
                              "Local" "Static"))

             (keywords-regex         (regexp-opt keywords 'words))
             (operators-regex (regexp-opt operators))
             (negation-char-regex    (regexp-opt '("!" "~")))
             (number-regex           (rx (one-or-more digit)))
             (string-regex           (rx "\"" (*? (not "\"")) "\""))
             (builtin-types-regex    (regexp-opt builtin-types 'words)))
        `((,keywords-regex          . ,int-isel-mode-face-keywords)
          (,builtin-types-regex     . ,int-isel-mode-face-types)
          (,number-regex            . ,int-isel-mode-face-numbers)
          (,operators-regex  . ,int-isel-mode-face-operators)
          (,negation-char-regex     . ,int-isel-mode-face-negation-char)
          (,string-regex            . ,int-isel-mode-face-strings)
          )))

(defcustom int-isel-mode-indent-amount 2
  "The amount of space characters that each level of parenthesis nesting
in Intercept source code will be indented."
  :group 'int-isel-mode)

(defun int-isel--indent-line ()
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
      (indent-to (* indent int-isel-mode-indent-amount)))
    (when should-move-eol
      (move-end-of-line nil))))

(define-derived-mode int-isel-mode prog-mode
  "intc ISel"
  (setq-local comment-start ";;")
  (setq-local comment-end   "")
  (setq font-lock-defaults '((int-isel--font-lock-defaults)))
  (setq indent-line-function #'int-isel--indent-line))

(provide 'int-isel-mode)

;;; int-isel-mode.el ends here
