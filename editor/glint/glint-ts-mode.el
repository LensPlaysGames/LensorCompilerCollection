;;; glint-ts-mode.el --- Major mode for editing Glint Programming Language (part of LCC) using tree-sitter syntax highlighting -*- lexical-binding: t -*-

;; Author: Lens_r
;; Maintainer: Lens_r
;; Version: 0.0.0
;; Package-Requires: ((emacs "29.1") (treesit))
;; Homepage: https://github.com/LensPlaysGames/LensorCompilerCollection
;; Keywords: glint, tree-sitter


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

;;; Code:

(require 'treesit)

(declare-function treesit-parser-create "treesit.c")

(defun glint-ts-mode--install ()
  "Use 'treesit-install-language-grammar' to install the Glint language grammar"
  (interactive)
  (let ((treesit-language-source-alist
         '((glint "https://github.com/LensPlaysGames/tree-sitter-glint"))))
    (treesit-install-language-grammar 'glint)))

(defun glint-ts-mode--font-lock-settings ()
  "Tree-sitter font-lock settings for Glint Programming Language."
  (treesit-font-lock-rules
   :default-language 'glint
   :feature 'comment
   `((comment) @font-lock-comment-face)

   :feature 'variable
   `((identifier) @font-lock-variable-name-face)

   :feature 'function
   `((declaration name: (identifier) @font-lock-function-name-face
                  type: (type_function))
     (declaration name: (identifier) @font-lock-function-name-face
                  type: (type_pointer (type_function))))

   :feature 'type
   `((type_fixed_array)   @font-lock-type-face
     (type_dynamic_array) @font-lock-type-face
     (type_ffi)           @font-lock-type-face
     (type_identifier)    @font-lock-type-face
     (type_pointer)       @font-lock-type-face
     (type_primitive)     @font-lock-type-face
     (type_reference)     @font-lock-type-face)

   :feature 'number
   `((number_literal) @font-lock-number-face)

   :feature 'string
   `((string) @font-lock-string-face)

   :feature 'keyword
   `(["discardable" "external" "export" "module" "return"] @font-lock-keyword-face)

   ;; :feature 'operator
   ;; `([] @font-lock-operator-face)

   :feature 'delimiter
   `(["(" ")" "{" "}"] @font-lock-delimiter-face)
   ))

(define-derived-mode glint-ts-mode prog-mode "Glint"
  "Major mode for the Glint programming language."
  :group "glint"

  ;; Attempt to automagically build + install grammar.
  (unless (treesit-ready-p 'glint t)
    (glint-ts-mode--install))

  (if (treesit-ready-p 'glint 'message)
      (progn
        (treesit-parser-create 'glint)

        (setq-local comment-start ";;")
        (setq-local comment-end "")

        ;; Font Lock
        (setq-local
         treesit-font-lock-settings
         (glint-ts-mode--font-lock-settings))
        (setq-local
         treesit-font-lock-feature-list
         '(( function comment string )
           ( keyword type )
           ( number operator delimiter )
           ( variable )))

        (treesit-major-mode-setup))
    (prog-mode)))

(when (treesit-ready-p 'glint t)
  (add-to-list 'auto-mode-alist '("\\.g\\'" . glint-ts-mode)))

(provide 'glint-ts-mode)

;;; glint-ts-mode.el ends here
