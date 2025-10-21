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

(defcustom
  glint-ts-mode-indent-offset 2
  "Amount of spaces to be used as a unit of indentation."
  :type 'integer)
;; FIXME: why is this here
(unless glint-ts-mode-indent-offset (setq glint-ts-mode-indent-offset 2))

(defvar glint-ts-mode--unary-node-names
  '("addressof" "dereference" "increment" "decrement" "negate" "logical_negate"))

(defvar glint-ts-mode--binary-node-names
  '("add" "subtract" "multiply" "divide" "remainder"
    "eq" "ne" "gt" "ge" "lt" "le"
    "and" "or"
    "bitshl" "bitshr" "bitand" "bitor" "bitxor"
    "add_eq" "subtract_eq" "multiply_eq" "divide_eq" "remainder_eq"
    "tilde_eq" "ampersand_eq" "pipe_eq" "caret_eq" "lbrack_eq"))


(defmacro glint-ts-mode--parent-is-unary ()
  "Returns a matcher meant to be used within 'treesit-simple-indent-rules'.
Basically, instead of writing out each binary node name, this expands it
from a list automagically using LISP macros...
See 'treesit-simple-indent-presets' for more info."
  '`(or ,@(mapcar (lambda (n) `(parent-is ,n)) glint-ts-mode--unary-node-names)))

(defmacro glint-ts-mode--parent-is-binary ()
  "Returns a matcher meant to be used within 'treesit-simple-indent-rules'.
Basically, instead of writing out each binary node name, this expands it
from a list automagically using LISP macros...
See 'treesit-simple-indent-presets' for more info."
  '`(or ,@(mapcar (lambda (n) `(parent-is ,n)) glint-ts-mode--binary-node-names)))

(defvar glint-ts-mode--indent-rules-lens
  `(
    ( ;; rule-begin
     ;; BLOCK CLOSER REMOVES INDENT
     (or (node-is "}"))
     standalone-parent ;; anchor
     0 ;; offset
     ) ;; rule-end

    ( ;; rule-begin
     ;; BLOCK EXPRESSION CAUSES INDENT
     (or (parent-is "block")) ;; matcher
     standalone-parent ;; anchor
     glint-ts-mode-indent-offset ;; offset
     ) ;; rule-end

    ( ;; rule-begin
     ;; CFOR KEYWORD CAUSES DOUBLE INDENT FOR INIT, CONDITION, INCREMENT
     (and (parent-is "cfor") (not (field-is "body")))
     standalone-parent ;; anchor
     ,(* 2 glint-ts-mode-indent-offset) ;; offset
     ) ;; rule-end

    ( ;; rule-begin
     ;; CFOR KEYWORD CAUSES SINGLE INDENT FOR BODY
     (and (parent-is "cfor") (field-is "body"))
     standalone-parent ;; anchor
     glint-ts-mode-indent-offset ;; offset
     ) ;; rule-end

    ( ;; rule-begin
     ;; FOR KEYWORD CAUSES SINGLE INDENT FOR BODY
     ;;     : for x in y,
     ;;     :   print x;
     ;; Presence of not "node-is block" toggles GNU style curly braces (2
     ;; spaces for braces on newline, 2 spaces for stuff inside braces).
     (and (parent-is "rangedfor") (field-is "body") (not (node-is "block")))
     standalone-parent ;; anchor
     glint-ts-mode-indent-offset ;; offset
     ) ;; rule-end
    ( ;; rule-begin
     ;; BLOCK EXPRESSION (FOR BODY) ON NEWLINE MATCHES INDENT
     ;;     : for x in y,
     ;;     : {
     ;;     : };
     (and (parent-is "rangedfor") (field-is "body") (node-is "block"))
     standalone-parent ;; anchor
     0 ;; offset
     ) ;; rule-end

    ( ;; rule-begin
     ;; DECLARATION INDENTS NON-BLOCK INIT EXPRESSION
     ;;     : foo : int()
     ;;     :   69;
     (and (parent-is "declaration") (field-is "init") (not (node-is "block")))
     standalone-parent ;; anchor
     glint-ts-mode-indent-offset ;; offset
     ) ;; rule-end
    ( ;; rule-begin
     ;; BLOCK EXPRESSION (DECLARATION INIT) ON NEWLINE MATCHES INDENT
     ;;     : foo : int()
     ;;     : {
     ;;     :   69;
     ;;     : }
     (and (parent-is "declaration") (field-is "init") (node-is "block"))
     standalone-parent ;; anchor
     0 ;; offset
     ) ;; rule-end

    ( ;; rule-begin
     ;; UNARY EXPRESSION CAUSES INDENT (if operand on new line)
     ,(glint-ts-mode--parent-is-unary)
     standalone-parent ;; anchor
     glint-ts-mode-indent-offset ;; offset
     ) ;; rule-end

    ( ;; rule-begin
     ;; BINARY EXPRESSION CAUSES INDENT (if rhs operand on new line)
     ,(glint-ts-mode--parent-is-binary)
     first-sibling ;; anchor
     glint-ts-mode-indent-offset ;; offset
     ) ;; rule-end

    )
  "\"Lens_r\" rules
Egyptian braces with contents indented one additional level from left-
most expression of brace line.

  while x, {
    ...
  };

If you do put a newline in-between a block and it's parent, it will
match the indentation level of it's parent.

  while x,
  {
    ...
  };

Single statements may or may not use braces, and are indented one
additional level.

  while x,
    69;
")

;; "Allman" rules
;; All braces begin on newline at same indentation,
;; with the braces' contents indented one level.
;; while x,
;; {
;;   contents
;; }
(defvar glint-ts-mode--indent-rules-allman
  '(

    (error "TODO")

    ( ;; rule-begin
     ) ;; rule-end

    )
  "\"Allman\" rules
Braces on newline with same indentation.
Block contents add a level of indentation.

  while x,
  {
    ...
  };


Single statements should use braces, otherwise their indentation is
undefined.
")

(defvar glint-ts-mode--indent-rules-gnu
  '(

    (error "TODO")

    ( ;; rule-begin
     ) ;; rule-end

    )
  "\"GNU\" rules
All braces begin on newline at one level of indentation,
with the braces' contents indented another level.

  while x,
    {
      ...
    };
")

(defvar glint-ts-mode--indent-rules-whitesmiths
  '(

    (error "TODO")

    ( ;; rule-begin
     ) ;; rule-end

    )
  "\"Whitesmiths\" rules
All braces begin on newline at one additional level of indentation,
with the braces' contents having the same indentation.

  while x,
    {
    contents
    }
")

(defvar glint-ts-mode--indent-rules-common
  '(

    ( ;; rule-begin
     ;; CHILDREN OF SOURCE FILE ARE NOT INDENTED
     (or (parent-is "source_file"))
     column-0 ;; anchor
     0 ;; offset
     ) ;; rule-end

    )
  "Indent rules shared by all Glint indent styles.")

(defcustom glint-ts-mode--indent-rules-style
  'glint-ts-mode--indent-rules-lens
  ""
  :type '(choice (variable-item glint-ts-mode--indent-rules-lens)
                 (variable-item glint-ts-mode--indent-rules-allman)
                 (variable-item glint-ts-mode--indent-rules-gnu)
                 (variable-item glint-ts-mode--indent-rules-whitesmiths)))

(defvar glint-ts-mode--indent-rules
  `((glint
     ,@glint-ts-mode--indent-rules-common
     ,@(symbol-value glint-ts-mode--indent-rules-style)))
  "See `treesit-simple-indent-rules' (with `M-x' `describe-variable'),
 as well as `treesit-simple-indent-presets': very helpful!.")

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
   `((declaration type: (identifier) @font-lock-type-face)
     ["enum" "struct" "sum" "union"] @font-lock-type-face
     (type_array)      @font-lock-type-face
     (type_ffi)        @font-lock-type-face
     (type_pointer)    @font-lock-type-face
     (type_primitive)  @font-lock-type-face
     (type_reference)  @font-lock-type-face
     (type_pointer_to_pointer)    @font-lock-type-face)

   :feature 'number
   `((integer_literal) @font-lock-number-face
     (bool_literal)    @font-lock-number-face
     (byte_literal)    @font-lock-number-face)

   :feature 'string
   `((string_literal) @font-lock-string-face)

   :feature 'keyword
   `([
      "import" "module"
      "if" "else" "while"
      "cfor" "for" "in"
      "not"
      "and" "or"
      "external" "export"
      "return"
      "supplant" "match" "print"
      ]
     @font-lock-keyword-face)

   :feature 'operator
   `(["@" "!" ; "~"
      "+" "-" "*" "/" "&" "|" "^" "=" "!=" "<" "<=" ">" ">="
      "::" ":" ":="
      "++" "--"
      "+=" "-=" "*=" "/=" "%=" "~=" "&=" "|=" "^=" "[="
      ]
     @font-lock-operator-face)

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
         treesit-simple-indent-rules
         glint-ts-mode--indent-rules)
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
  (add-to-list 'auto-mode-alist '("\\.g\\'" . glint-ts-mode))
  (with-eval-after-load 'org
    (add-to-list 'org-src-lang-modes '("glint" . glint-ts))))

(provide 'glint-ts-mode)

;;; glint-ts-mode.el ends here
