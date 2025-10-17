;;; runtest.el --- Run programs compiled with LCC, asserting their output and status -*- lexical-binding: t -*-

;; Author: Lens_r
;; Maintainer: Lens_r
;; Version: 0.0.1
;; Package-Requires: ()
;; Homepage: www.github.com/LensPlaysGames/LensorCompilerCollection
;; Keywords: LCC, Glint


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

;;; Code:

(defcustom
  lcc-path "/home/lens_r/Programming/LensorCompilerCollection/bld/lcc"
  "The path to the LCC compiler on your system")

(defvar run-test--completed-list ()
  "List of completed tests")
(setq run-test--completed-list ())

(defun run-test--completed (test-name)
  (memq test-name run-test--completed-list))

(defun run-test--mark-completed (test-name)
  (push test-name run-test--completed-list))

(defun run-test--execute (executable-filepath test-name expected-status expected-output)
  "Run executable"
  (defvar exe-output "")
  (setq exe-output "")
  (make-process
   :name "exe"
   :buffer nil
   :command (list executable-filepath)
   :filter
   (lambda (p o)
     (setq exe-output (concat exe-output o)))
   :sentinel
   (lambda (p e)
     (when (eq 'exit (process-status p))
       ;; (message "    executed %s (%i)" (file-relative-name executable-filepath) (process-exit-status p))
       (delete-file executable-filepath)
       (when (not (= (process-exit-status p) expected-status))
         (message "    UNEXPECTED STATUS\n\tEXPECTED %i\n\tGOT %i" expected-status (process-exit-status p)))
       (when (not (string-equal exe-output expected-output))
         (message "    UNEXPECTED OUTPUT\n\tEXPECTED:\n'%s'\n\tGOT:\n'%s'" expected-output exe-output))
       (when (and (= (process-exit-status p) expected-status) (string-equal exe-output expected-output))
         (message "    PASSED"))
       (run-test--mark-completed test-name)))))

(defun run-test--gcc (source-filepath test-name expected-status expected-output)
  "Run GCC"
  (defvar output-filepath "")
  (defvar gcc-output "")
  (setq gcc-output "")
  (setq output-filepath
        (file-name-with-extension (make-temp-name (expand-file-name "a")) "out"))
  (make-process
   :name "gcc"
   :buffer nil
   :command (list "gcc" source-filepath "-o" output-filepath)
   :filter
   (lambda (p o)
     (setq gcc-output (concat gcc-output o)))
   :sentinel
   (lambda (p e)
     (when (eq 'exit (process-status p))
       ;; (message "    gcc compiled %s into %s"
       ;;          (file-relative-name source-filepath)
       ;;          (file-relative-name output-filepath))
       (delete-file source-filepath)
       (if (= (process-exit-status p) 0)
           (run-test--execute output-filepath test-name expected-status expected-output)
         (error "GCC returned non-zero exit code (%i) %s" (process-exit-status p) gcc-output))))))

(defun run-test--lcc (language source-filepath test-name expected-status expected-output)
  "Run LCC"
  (defvar output-filepath ""
    "The filepath where LCC will write its output to.")
  (defvar lcc-output ""
    "A string storing the stdout+stderr of LCC.")
  (setq output-filepath
        (file-name-with-extension (make-temp-name (expand-file-name "./lcctmp")) "s"))
  (setq lcc-output "")
  (make-process
   :name "lcc"
   :buffer nil
   :command (list lcc-path source-filepath "-x" language "-o" output-filepath)
   :filter
   (lambda (p o)
     (setq lcc-output (concat lcc-output o)))
   :sentinel
   (lambda (p e)
     (when (eq 'exit (process-status p))
       ;; (message "    lcc compiled %s into %s"
       ;;          (file-relative-name source-filepath)
       ;;          (file-relative-name output-filepath))
       (if (= 0 (process-exit-status p))
           (progn
             (sleep-for 0.1) ;; allow file LCC writes to "appear"
             (run-test--gcc output-filepath test-name expected-status expected-output))
         (error "LCC returned non-zero exit code (%i) %s" (process-exit-status p) lcc-output))))))

(defun run-test--file (language test-name source-filepath expected-status expected-output)
  ""
  (message "Running %s Test %s" language (file-relative-name source-filepath))
  (let ((proc (run-test--lcc language source-filepath test-name expected-status expected-output)))
    ;; Wait until the test has been marked as completed...
    (while (not (run-test--completed test-name))
      (sleep-for 0.05))))

;; (mapc
;;  (lambda (name) (run-test--file "glint" name 0 ""))
;;  (directory-files "./runtest/corpus/glint" t directory-files-no-dot-files-regexp))

(defun run-test--glint-tests ()
  ""
;; TODO: Parse tests from file(s)
  (message "Running Glint Tests")
  (run-test--file
   "glint"
   "Top Level Implicit Integer Return"
   "corpus/glint/toplevel_implicit_integer_return.txt"
   69 "")
  (run-test--file
   "glint"
   "Dynamic Array Operations"
   "corpus/glint/dynamic_array_operations.g"
   0 "4\n2\n0\n\n420\n"))

(defun run-test--main ()
  ""
  (run-test--glint-tests))

(run-test--main)

(provide 'runtest)

;;; runtest.el ends here
