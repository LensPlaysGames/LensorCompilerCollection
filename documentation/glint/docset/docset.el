;;; docset.el --- Zeal/Dash Docset Generation from Org Source(s) -*- lexical-binding: t -*-

;; Author: Lens_r
;; Maintainer: Lens_r
;; Version: 0.0.1
;; Package-Requires: ()
;; Homepage: https://github.com/LensPlaysGames/LensorCompilerCollection
;; Keywords: zeal, glint, documentation


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

;; This is made so that we can generate a Glint docset.

;;; Code:

(require 'sqlite)

(defun docset-install--zeal (docset-path)
  ;; TODO: Ensure path is a directory that ends in ".docset"
  (unless (file-directory-p docset-path)
    (error "You know what you did"))

  ;; Delete existing docset directory, if it exists
  ;; TODO: Ensure docset-path is different from the deleted directory's path
  ;; (before deletion).
  (let
      ((docsets-install-path
        (if (or (eq "windows-nt" system-type)
                (eq "cygwin" system-type)
                (eq "ms-dos" system-type))
            "$HOME/AppData/Local/Zeal/Zeal/docsets/"
          "~/.local/share/Zeal/Zeal/docsets/")))
    (when (file-exists-p docsets-install-path)
      (delete-directory
       (concat docsets-install-path
               (file-name-nondirectory docset-path))
       t))

    ;; Copy given directory and all of it's contents to Zeal docset location.
    (copy-directory docset-path docsets-install-path)))

(defun docset-db-open ()
  "Return a sqlite database object referring to the Docset database.
Must be closed via 'docset-db-close' or similar."
  ;; TODO: Non-hard-coded path
  (let ((db-path "../Glint.docset/Contents/Resources/docSet.dsidx"))
    (when (file-exists-p db-path)
      (delete-file db-path))
    (let
        ((db (sqlite-open db-path)))
      (sqlite-execute-batch
       db
       (concat
        "CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT);"
        "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path);"))
      db)))

(defun docset-db-search-entry (db search-term type path)
  "Define an entry in 'db' such that 'search-term' resolves to 'path'.
Recorded as type 'type', with 'type' being one of:
  https://kapeli.com/docsets#supportedentrytypes"
  (sqlite-execute
   db "INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES (?, ?, ?);"
   (list search-term type path)))

(defun docset-db-close (db)
  "Close an open database, ensuring transactions are flushed."
  (sqlite-close db))

(defun docset-org-to-html (path)
  "Convert org file at 'path' to HTML using 'org-html-export-to-html'.
Return the absolute file path of the generated HTML."
  (unless (and (stringp path)
               (string-suffix-p ".org" path)
               (file-exists-p path))
    (error "Invalid path to ORG file: %s" path))
  (let
      ((out-directory (expand-file-name
                       "../Glint.docset/Contents/Resources/Documents/")))
    (unless (file-exists-p out-directory)
      (error "Cannot generate HTML as we don't know where the docset is!"))
    (let
        ((copied-source-path (expand-file-name (file-name-nondirectory path)
                                               out-directory)))
      ;; Copy source to docset output directory
      ;; (message "Copying %s to %s" path copied-source-path)
      (copy-file path copied-source-path t)
      ;; Delete the copied source after generation...
      (let
          ((result (with-current-buffer (find-file-noselect copied-source-path t)
                     ;; Generate HTML from copied source
                     (expand-file-name (org-html-export-to-html)))))
        (kill-buffer (get-file-buffer copied-source-path))
        (delete-file copied-source-path)
        result))))

(defun docset-make ()
  ;; Configure ORG HTML export (i.e. 'org-html-head').
  ;; Create new search database to populate with entries.
  (let
      ((db (docset-db-open))
       (org-html-head
        (concat (if (boundp 'org-html-head) org-html-head "")
                "<link rel=\"stylesheet\" href=\"all.css\">"))
       ;; Don't insert "validate" footer stuff
       (org-html-postamble nil)
       ;; Use CSS classes to colorize output.
       (org-html-htmlize-output-type 'css)
       ;; Disable default CSS output (we have a custom theme).
       (org-html-head-include-default-style nil)
       ;; Do not export a table of contents unless specified via "toc:t" or
       ;; similar in "#+options" of source file.
       (org-export-with-toc nil))

    ;; Install CSS
    (copy-file "src/all.css"
               "../Glint.docset/Contents/Resources/Documents/all.css"
               t)

    ;; The Book -> HTML
    (docset-org-to-html "../thebook.org")
    (docset-db-search-entry db
                            "The Book" "Guide"
                            "thebook.html")

    ;; Guides -> HTML
    (mapc
     (lambda (guide-path)
       ;; Convert each guide to HTML.
       (let
           ((guide-html-path (docset-org-to-html guide-path)))
         ;; Create an entry in search database.
         ;; TODO: Extract title from org source.
         (docset-db-search-entry db
                                 (file-name-base guide-path)
                                 "Guide"
                                 (file-name-nondirectory guide-html-path))))
     (directory-files "src/guides" 'absolute "\\.org\\'"))

    ;; TODO
    ;; We want each function of the standard library.
    ;;     src/std/*.org
    ;; We want built-ins like =print=.
    ;;     src/builtins/*.org
    ;; We want function attributes like =discardable=.
    ;;     src/attributes/*.org
    ;; We want keywords like =if=.
    ;;     src/keywords/*.org
    ;; We want constants like =true=.
    ;;     src/constants/*.org
    ;; We want operators like =+=.
    ;;     src/operators/*.org
    ;; We want modules like =std.io=.
    ;;     src/modules/*.org
    ;; We want errors like =expected=.
    ;;     src/errors/*.org

    ;; index.org -> HTML
    (docset-org-to-html "./src/index.org")
    (docset-db-search-entry db "Home" "File" "index.html")

    (docset-db-close db)))

;; (docset-make)
;; (docset-install--zeal "../Glint.docset")

(provide 'docset)

;;; docset.el ends here
