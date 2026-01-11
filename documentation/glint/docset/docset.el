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

;; commentary

;;; Code:

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
    (delete-directory
     (concat docsets-install-path
             (file-name-nondirectory docset-path))
     t)

    ;; Copy given directory and all of it's contents to Zeal docset location.
    (copy-directory docset-path docsets-install-path)))

;; TODO: The idea is to have some sort of org source(s) containing
;; documentation for Glint, and to automagically generate the HTML,
;; as well as the sqlite searchIndex database.
;;
;; We want The Book as a Guide.
;; We want each function of the standard library.
;; We want built-ins like =print=.
;; We want function attributes like =discardable=.
;; We want keywords like =if=.
;; We want constants like =true=.
;;
;; Eventually, we want little one-off explainers on Glint concepts as
;; Guides. i.e. one for default initialization, one for how to represent
;; trees in Glint using the built-in sum and dynamic array types, etc.

(provide 'docset)

;;; docset.el ends here
