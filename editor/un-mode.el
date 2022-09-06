;;; un-mode.el --- A major mode for editing the unnamed language that the unnamed compiler compiles... -*- lexical-binding: t -*-

;;; Code:

(defvar un-mode-syntax-table
  (make-syntax-table)
  "Syntax table for un-mode")

;; Set semi-colon as comment starting character.
(modify-syntax-entry
 ?\; "<"
 un-mode-syntax-table)
;; Set hash/pound as comment starting character.
(modify-syntax-entry
 ?# "<"
 un-mode-syntax-table)
;; Set newline as comment ending character.
(modify-syntax-entry
 ?\n ">"
 un-mode-syntax-table)

;; Delimiters include comma
(modify-syntax-entry
 ?, "."
 un-mode-syntax-table)

;; Allowed within symbols: _-#$
(modify-syntax-entry
 ?_ "_"
 un-mode-syntax-table)
(modify-syntax-entry
 ?- "_"
 un-mode-syntax-table)
(modify-syntax-entry
 ?# "_"
 un-mode-syntax-table)
(modify-syntax-entry
 ?$ "_"
 un-mode-syntax-table)

;; Parenthesis
(modify-syntax-entry
 ?\( "()"
 un-mode-syntax-table)
(modify-syntax-entry
 ?\) ")("
 un-mode-syntax-table)
(modify-syntax-entry
 ?\[ "(]"
 un-mode-syntax-table)
(modify-syntax-entry
 ?\] ")["
 un-mode-syntax-table)
(modify-syntax-entry
 ?{ "(}"
 un-mode-syntax-table)
(modify-syntax-entry
 ?} "){"
 un-mode-syntax-table)

;; Unary prefix operators
;; Addressof
(modify-syntax-entry
 ?& "'"
 un-mode-syntax-table)
;; Dereference
(modify-syntax-entry
 ?@ "'"
 un-mode-syntax-table)

;; Gather all keyword font locks together into big daddy keyword font-lock
(setq un--font-lock-defaults
      (let* ((keywords '("if" "else"))
             (binary-operators '("+" "*" "-" "/"
                                 "<" ">"
                                 ":" "=" ":="
                                 "&" "@"))

             (keywords-regex (regexp-opt keywords 'words))
             (binary-operators-regex (regexp-opt binary-operators 'words))
             (builtin-types-regex (rx (zero-or-more "@")
                                      (or "integer")))
             )
        `((,keywords-regex . 'font-lock-keyword-face)
          (,builtin-types-regex . 'font-lock-type-face)
          (,binary-operators-regex . nil)
          )))

(defcustom un-mode-indent-amount 2
  "The amount of characters that each level of parenthesis nesting in the unnamed language source code will be indented."
  :group 'un-mode)

(defun un--indent-line ()
  "Indent a line in the unnamed programming language Lens made for fun."
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
      (indent-to (* indent un-mode-indent-amount)))
    (when should-move-eol
      (move-end-of-line nil))))

(define-derived-mode un-mode prog-mode
  "unnamed"
  (setq font-lock-defaults '(un--font-lock-defaults))
  (setq indent-line-function #'un--indent-line))

(provide 'un-mode)

;;; un-mode.el ends here
