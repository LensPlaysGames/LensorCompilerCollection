;;; un-ts-mode.el --- Syntax highlighting like un-mode but better because tree-sitter -*- lexical-binding: t -*-

;; Author: Rylan Lens Kellogg
;; Maintainer: Rylan Lens Kellogg
;; Version: 0.0.1
;; Keywords   : FUNCompiler languages tree-sitter


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'treesit)

(declare-function treesit-parser-create "treesit.c")
;(declare-function treesit-induce-sparse-tree "treesit.c")
;(declare-function treesit-node-parent "treesit.c")
;(declare-function treesit-node-start "treesit.c")
;(declare-function treesit-node-end "treesit.c")
;(declare-function treesit-node-child "treesit.c")
;(declare-function treesit-node-child-by-field-name "treesit.c")
;(declare-function treesit-node-type "treesit.c")


(defvar un-ts-mode--syntax-table
  (make-syntax-table)
  "Syntax table for un-ts-mode")

;; Set semi-colon as comment starting character.
(modify-syntax-entry
 ?\; "<"
 un-ts-mode--syntax-table)
;; Set hash/pound as comment starting character.
(modify-syntax-entry
 ?# "<"
 un-ts-mode--syntax-table)
;; Set newline as comment ending character.
(modify-syntax-entry
 ?\n ">"
 un-ts-mode--syntax-table)

;; Delimiters include comma
(modify-syntax-entry
 ?, "."
 un-ts-mode--syntax-table)

;; Allowed within symbols: _-#$
(modify-syntax-entry
 ?_ "_"
 un-ts-mode--syntax-table)
(modify-syntax-entry
 ?- "_"
 un-ts-mode--syntax-table)
(modify-syntax-entry
 ?# "_"
 un-ts-mode--syntax-table)
(modify-syntax-entry
 ?$ "_"
 un-ts-mode--syntax-table)

;; Parenthesis
(modify-syntax-entry
 ?\( "()"
 un-ts-mode--syntax-table)
(modify-syntax-entry
 ?\) ")("
 un-ts-mode--syntax-table)
(modify-syntax-entry
 ?\[ "(]"
 un-ts-mode--syntax-table)
(modify-syntax-entry
 ?\] ")["
 un-ts-mode--syntax-table)
(modify-syntax-entry
 ?{ "(}"
 un-ts-mode--syntax-table)
(modify-syntax-entry
 ?} "){"
 un-ts-mode--syntax-table)

;; Unary prefix operators
;; Addressof
(modify-syntax-entry
 ?& "'"
 un-ts-mode--syntax-table)
;; Dereference
(modify-syntax-entry
 ?@ "'"
 un-ts-mode--syntax-table)


(defvar un-ts-mode--keywords
  '("if" "else" "ext")
  "un keywords for tree-sitter font-locking.")

(defvar un-ts-mode--operators
  '(  "+" "-" "*" "/" "%"
      "&"
      "<<"
      ">>"
      "="
      "<"
      ">"
      ":"
      ":="
      "@"
    )
  "un operators for tree-sitter font-locking.")

(defvar un-ts-mode--delimiters
  '("(" ")"
    "[" "]"
    "{" "}"
    )
  "un delimiters for tree-sitter font-locking.")

(defun un-ts-mode--font-lock-settings()
  "Tree-sitter font-lock settings."
  (treesit-font-lock-rules
   :language 'un
   :feature 'type
   `((type_primitive) @font-lock-type-face
     (type_array)     @font-lock-type-face
     (type_pointer)   @font-lock-type-face
     )
   :language 'un
   :feature 'keyword
   `([,@un-ts-mode--keywords] @font-lock-keyword-face)
   :language 'un
   :feature 'operator
   `([,@un-ts-mode--operators] @font-lock-operator-face)
   :language 'un
   :feature 'punctuation
   `([,@un-ts-mode--delimiters] @font-lock-delimiter-face)
   ;; TODO: Figure out why variable query breaks things...
   ;;:language 'un
   ;;:feature 'variable
   ;;`((variable) @font-lock-variable-face)
   :language 'un
   :feature 'function
   `((expr_call
      callee: (variable) @font-lock-function-name-face)
     (stmt_function
      name: (identifier) @font-lock-function-name-face)
     )
   :language 'un
   :feature 'number
   `((number) @font-lock-number-face)
   :language 'un
   :feature 'comment
   `((comment) @font-lock-comment-face)
   ))

;;;###autoload
(define-derived-mode un-ts-mode--base-mode prog-mode "un"
  "Major mode for editing un, powered by tree-sitter. "
  :syntax-table un-ts-mode--syntax-table

  ;; Electric
  (setq-local electric-indent-chars
              (append "{}():," electric-indent-chars))

  (setq-local treesit-font-lock-feature-list
              '(( function variable comment )
                ( keyword type)
                ( number operator punctuation )
                ( ))))

;;;###autoload
(define-derived-mode un-ts-mode un-ts-mode--base-mode "un"
  "Major mode for editing un, powered by tree-sitter."
  :group 'un

  (unless (treesit-ready-p 'un)
    (error "Tree-sitter for C isn't available"))

  (treesit-parser-create 'un)

  ;; Comments.
  (setq-local comment-start ";;")
  (setq-local comment-end   "")

  ;; TODO: What do we set this to?
  ;;(setq-local treesit-simple-indent-rules nil)

  ;; Font-lock.
  (setq-local treesit-font-lock-settings
              (un-ts-mode--font-lock-settings))

  (treesit-major-mode-setup))


(provide 'un-ts-mode)

;;; un-ts-mode.el ends here
