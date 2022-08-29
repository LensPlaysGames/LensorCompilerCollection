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

(defconst un--font-lock-operators
  (list
   ;; Matching Regexp
   (rx (or
        "+" "*" "-" "/"
        "<" ">"
        ":" "=" ":="
        "&" "@"
        ))
   '(0 nil)
   ))

(defconst un--font-lock-generic
  (list
   ;; Matching Regexp
   (rx (or "defun" "if" "else"))
   ;; Font face to use
   '(0 font-lock-keyword-face)
   ))

(defconst un--font-lock-function-name
  (list
   ;; Matching Regexp
   (rx "defun" whitespace
       (group (* (or (not whitespace) "("))))
   ;; Font face to use
   '1
   'font-lock-function-name-face
   ))

(defconst un--font-lock-builtin-types
  (list
   ;; Matching Regexp
   (rx (zero-or-more "@")
       (or "integer" "function"))
   ;; Font face to use
   '(0 font-lock-type-face)
   ))

;; Gather all keyword font locks together into big daddy keyword font-lock
(defconst un--font-lock
  (list
   un--font-lock-builtin-types
   un--font-lock-operators
   un--font-lock-function-name
   un--font-lock-generic
   ))

(define-derived-mode un-mode prog-mode
  "unnamed"
  (setq font-lock-defaults '(un--font-lock)))

;;; un-mode.el ends here
