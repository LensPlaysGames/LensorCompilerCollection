(add-to-list 'auto-mode-alist '("\\.ir\\'" . un-ir-mode))

(defconst *un-ir-ident* "[a-zA-Z_][a-zA-Z0-9_]*")
(defvar un-ir-mode-font-lock-keywords
  `((
     (";.*" . font-lock-comment-face)
     ("[\"'].*?[\"']" . font-lock-string-face)
     (,(concat "\\(" *un-ir-ident* "\\)[[:space:]]*\\((\\)") (1 font-lock-string-face) (2 font-lock-keyword-face))
     ("\\<defun\\>" . font-lock-keyword-face)
     ("br.cond" . font-lock-function-name-face)
     ("\\<\\(consteval\\|forceinline\\|global\\|noreturn\\|pure\\|leaf\\)\\>" . font-lock-keyword-face)
     ("\\<\\(call\\|tail\\|br\\|unreachable\\|sub\\|add\\|mul\\|div\\|mod\\|ret\\|eq\\|lt\\|ne\\|gt\\|le\\|ge\\)\\>" . font-lock-function-name-face)
     (,(concat "\\(%[a-zA-Z0-9_]+\\)\\>") . font-lock-variable-name-face)
     (,(concat "\\<\\(" *un-ir-ident* "\\)\\>") . font-lock-warning-face)
;    ("\\<\\([iu]\\(8\\|16\\|32\\|64\\|sz\\)\\|f32\\|f64\\|bool\\)\\>" . font-lock-keyword-face)
     ("\\<0\\s*\\|\\(0[bBoOxX]\\)?[0-9a-fA-F]+\\_>" . font-lock-constant-face)
     ("\\[\\|\\]\\|[|<>=+-/*#~&:;,{}()^!?]" . font-lock-keyword-face)
     )))
(defun un-ir-mode-indent-line () (insert "    "))

(define-derived-mode un-ir-mode prog-mode "un-ir"
    "Major mode for editing the language that FUNCompiler compiles intermediate representation."
    (setq font-lock-defaults un-ir-mode-font-lock-keywords)
    (setq comment-start ";")
    (setq comment-end "")
    (set (make-local-variable 'indent-line-function) 'un-ir-mode-indent-line))

(provide 'un-ir-mode)
