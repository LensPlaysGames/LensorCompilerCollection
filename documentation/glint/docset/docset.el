;;; docset.el --- Zeal/Dash Docset Generation from Org Source(s) -*- lexical-binding: t -*-

;; Author: Lens_r
;; Maintainer: Lens_r
;; Version: 0.0.1
;; Package-Requires: ((emacs 29.1))
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

(require 'org) ;; org-map-entries
(require 'org-element) ;; org-element-map
(require 'ox) ;; org export
(require 'ox-html) ;; org export, HTML
(require 'seq) ;; seq-count
(require 'simple) ;; open-line
(require 'sqlite)

(defun docset-get-first-org-heading-title ()
  "Return the title of the first heading in the current org buffer."
  (interactive)
  (let* ((data (org-element-parse-buffer 'greater-elements))
         (first-headline (org-element-map data 'headline
                           (lambda (el) el) nil 'first-match 'no-recursion)))
    (when first-headline
      (org-element-property :raw-value first-headline))))

(defun docset-get-orgfile-title (path)
  "Return the title of the first heading in the org file at PATH."
  (let ((title (with-current-buffer (find-file-noselect path)
                 (docset-get-first-org-heading-title))))
    (kill-buffer (get-file-buffer path))
    title))

;; https://amitp.blogspot.com/2021/04/automatically-generate-ids-for-emacs.html
(defun docset-title-to-filename (title)
  "Convert TITLE to a reasonable filename.
No spaces, no non-ascii, less symbols."
  ;; Based on the slug logic in org-roam, but org-roam also uses a
  ;; timestamp, and this uses only the slug.
  (setf title (downcase title))
  ;; Convert lots of things to '-'
  (setf title (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" title))
  (setf title (replace-regexp-in-string "-+" "-" title))
  (setf title (replace-regexp-in-string "^-" "" title))
  (setf title (replace-regexp-in-string "-$" "" title)))

(defun docset-org-generate-custom-ids ()
  "Generate CUSTOM_ID for any headings that are missing one."
  (interactive)
  (let ((existing-ids (org-map-entries
                       (lambda () (org-entry-get nil "CUSTOM_ID"))))
        (inserted-ids nil))
    (org-map-entries
     (lambda ()
       (let* ((custom-id      (org-entry-get nil "CUSTOM_ID"))
              (heading        (org-heading-components))
              (level          (nth 0 heading))
              (todo           (nth 2 heading))
              (headline       (nth 4 heading))
              (slug           (docset-title-to-filename headline))
              (preexisting-id (member slug existing-ids))
              (duplicate-id   (seq-count
                               (lambda (elem) (equal elem slug))
                               inserted-ids)))
         (when (and (not custom-id) (< level 4)
                    (not todo) (not preexisting-id))
           (when (> duplicate-id 0)
             ;; Push unchanged slug so we know how many duplicates of
             ;; each we've had: foo-1, foo-2, etc.
             (push slug inserted-ids)
             (setf slug (format "%s-%s" slug duplicate-id)))
           (org-entry-put nil "CUSTOM_ID" slug)
           (push slug inserted-ids)))))))

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
                     ;; Insert #+TITLE, if one doesn't exist already,
                     ;; with the contents of the first headline.
                     (unless (org-get-title)
                       (beginning-of-buffer) (open-line 2)
                       (insert "#+title: " (docset-get-first-org-heading-title)))
                     ;; Insert CUSTOM_IDs based on heading contents
                     (docset-org-generate-custom-ids)
                     (save-buffer)
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
       ;; Don't insert #+title, just use it for HTML metadata.
       (org-export-with-title nil)
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
         (docset-db-search-entry db
                                 (docset-get-orgfile-title guide-path)
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
    (copy-file "./src/index.in.org" "./index.org" t)
    (with-current-buffer (find-file-noselect "./index.org")
      (end-of-buffer) (newline) (open-line 1)
      (insert "** Guides\n")
      ;; file:///home/lens_r/Programming/play/LensorCompilerCollection/documentation/glint/docset/src/guides/build_process.html
      (mapc (lambda (guide-path)
              (insert (org-link-make-string
                       (concat
                        "file:"
                        (file-name-with-extension
                         (file-name-nondirectory guide-path)
                         "html"))
                       (docset-get-orgfile-title guide-path)))
              (newline 2))
            (directory-files "src/guides" 'absolute "\\.org\\'"))
      (save-buffer))
    (kill-buffer (get-file-buffer "./index.org"))
    (docset-org-to-html "./index.org")
    (delete-file "./index.org")
    (docset-db-search-entry db "Home" "File" "index.html")

    (docset-db-close db))
  "Generated docset")

;; (docset-make)
;; (docset-install--zeal "../Glint.docset")

(provide 'docset)

;;; docset.el ends here
