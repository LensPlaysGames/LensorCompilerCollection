;;; int-mode.el --- A major mode for editing Intercept source code -*- lexical-binding: t -*-

;;; Code:

(defvar int-mode-syntax-table
  (make-syntax-table)
  "Syntax table for int-mode")

;; Set semi-colon as comment starting character.
(modify-syntax-entry
 ?\; "<"
 int-mode-syntax-table)
;; Set hash/pound as comment starting character.
(modify-syntax-entry
 ?# "<"
 int-mode-syntax-table)
;; Set newline as comment ending character.
(modify-syntax-entry
 ?\n ">"
 int-mode-syntax-table)

;; Delimiters include comma
(modify-syntax-entry
 ?, "."
 int-mode-syntax-table)

;; Allowed within symbols: _-#$
(modify-syntax-entry
 ?_ "_"
 int-mode-syntax-table)
(modify-syntax-entry
 ?- "_"
 int-mode-syntax-table)
(modify-syntax-entry
 ?# "_"
 int-mode-syntax-table)
(modify-syntax-entry
 ?$ "_"
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

(defcustom int-mode-face-numbers nil
  "Symbol name of face to highlight int-mode immediate numbers with."
  :group 'int-mode
  :tag "Syntax Highlight Numbers")

(defcustom int-mode-face-operators nil
  "Symbol name of face to highlight int-mode builtin binary operators with.
Examples include addition (+) and subtraction (-)."
  :group 'int-mode
  :tag "Syntax Highlight Binary Operators")

(defcustom int-mode-face-negation-char 'font-lock-negation-char-face
  "Symbol name of face to highlight int-mode negation character (!) with."
  :group 'int-mode
  :tag "Syntax Highlight Negation Character")

(defcustom int-mode-face-function-name 'font-lock-function-name-face
  "Symbol name of face to highlight int-mode function names in definitions."
  :group 'int-mode
  :tag "Syntax Highlight Function Names")

;; Gather all keyword font locks together into big daddy keyword font-lock
(setq int--font-lock-defaults
      (let* ((keywords '("if" "else" "ext" "while"))
             (binary-operators '("+" "*" "-" "/" "%"
                                 "<" ">"
                                 ":" "=" ":="
                                 "&" "@"
                                 ">>" "<<" "&" "|" "~"))

             (keywords-regex         (regexp-opt keywords 'words))
             (binary-operators-regex (regexp-opt binary-operators))
             (negation-char-regex    (regexp-opt '("!")))
             (number-regex           (rx (one-or-more digit)))
             (builtin-types-regex    (rx (zero-or-more "@")
                                         (or "integer" "void")))
             (function-name-regex
              (rx (seq
                   ;; Function name
                   (group (+? anything))
                   ;; Whitespace between function name and type operator
                   (*? whitespace)
                   ;; The type operator, a colon (:)
                   (minimal-match ?:)
                   ;; Whitespace between : and type
                   (*? whitespace)
                   ;; Type name
                   (+? anything)
                   ;; Whitespace between type and parameters
                   (*? whitespace)
                   ;; Parameter list start, open parenthesis: (
                   (minimal-match ?\()
                   ;; Parameters
                   (+? anything)
                   ;; Parameter list end, close parenthesis: )
                   (minimal-match ?\))
                   )))
             )
        `(
          (,keywords-regex          . ,int-mode-face-keywords)
          (,builtin-types-regex     . ,int-mode-face-types)
          (,number-regex            . ,int-mode-face-numbers)
          (,binary-operators-regex  . ,int-mode-face-operators)
          (,negation-char-regex     . ,int-mode-face-negation-char)
          (,function-name-regex     . (1 ,int-mode-face-function-name))
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
  (setq font-lock-defaults '(int--font-lock-defaults))
  (setq indent-line-function #'int--indent-line))

(provide 'int-mode)

;;; int-mode.el ends here
