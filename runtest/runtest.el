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

;; For parsing test declarations written in Org-mode.
(require 'org-element)

(defcustom
  lcc-path "/home/lens_r/Programming/play/LensorCompilerCollection/dbg/lcc"
  "The path to the LCC compiler on your system")

(defvar run-test--completed-list ()
  "List of completed tests")

(defvar run-test--passed-list ()
  "List of completed tests that passed their assertions")

(defun run-test--completed (test-name)
  (memq test-name run-test--completed-list))

(defun run-test--mark-completed (test-name)
  (push test-name run-test--completed-list))

(defun run-test--mark-passed (test-name)
  (push test-name run-test--passed-list))

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

       ;; Once the program has exited, we no longer need it's executable...
       (delete-file executable-filepath)

       ;; Compare program exit status to expected program exit status.
       (when (not (= (process-exit-status p) expected-status))
         (message "UNEXPECTED STATUS: %s\n\tEXPECTED %i\n\tGOT %i"
                  test-name expected-status (process-exit-status p)))
       ;; Compare program output to expected program output.
       ;; TODO: Better visualisation of what's different, invisible characters, etc.
       (when (not (string-equal exe-output expected-output))
         (message "UNEXPECTED OUTPUT: %s\n\tEXPECTED:\n'%s'\n\tGOT:\n'%s'"
                  test-name expected-output exe-output))
       ;; Record passed/failed, and print a pretty message.
       (if (and (= (process-exit-status p) expected-status) (string-equal exe-output expected-output))
           (progn
             (run-test--mark-passed test-name)
             (message "PASSED: %s" test-name))
         (message "FAILED: %s" test-name))
       ;; Mark test as completed
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
             (sleep-for 0.02) ;; allow file LCC writes to "appear"
             (run-test--gcc output-filepath test-name expected-status expected-output))
         (error "LCC returned non-zero exit code (%i) %s" (process-exit-status p) lcc-output))))))

(defun run-test--wait-for-test-completion (test-name)
  "Sit until a test is registered as completed"
  (while (not (run-test--completed test-name))
      (sit-for 0.01)))

(defun run-test--file (language test-name source-filepath expected-status expected-output)
  ""
  ;; (message "Running %s Test %s (at %s)" language test-name (file-relative-name source-filepath))
  (run-test--lcc language source-filepath test-name expected-status expected-output)
  ;; Wait until the test has been marked as completed...
  (run-test--wait-for-test-completion test-name))

(defun run-test--parse-test-from-org (org-filepath)
  "Returns a property list containing information about the parsed test, or nil
 if parsing did not succeed.
Properties:
:name    Human-readable name
:source  Input source code
:status  Expected status from compiled+executed test
:output  Expected output from compiled+executed test

Additional properties are included, but not necessary:
:path    Path to org-file containing test declaration"
  (when (not (file-name-absolute-p org-filepath))
    (error "Expected absolute filepath to parse org file from..."))

  (defvar test ()
    "A property list containing information about the parsed test.")
  (setq test
        '(:name "" :source nil :status nil :output nil :path org-filepath))

  (with-current-buffer (find-file-noselect org-filepath)
    (let ((parsetree (org-element-parse-buffer)))
      (org-element-map
          parsetree
          t
        (lambda (elem)
          ;; (message "%s" (org-element-interpret-data elem))
          ;; (message "Contents:-->%s<--" elem)
          (when (eq (org-element-type elem) 'headline)
            (setq test
                  (plist-put
                   test :name (org-element-interpret-data (org-element-property :title elem)))))
          (when (string-equal (org-element-property :name elem) "source")
            (setq test
                  (plist-put
                   test :source (org-element-property :value elem))))
          (when (string-equal (org-element-property :name elem) "status")
            (setq test
                  (plist-put
                   test :status (string-to-number (org-element-property :value elem)))))
          (when (string-equal (org-element-property :name elem) "output")
            (setq test
                  (plist-put
                   test :output (org-element-property :value elem))))))))
  (let ((invalid-name (string-empty-p (plist-get test :name)))
        (invalid-source (not (plist-get test :source)))
        (invalid-status (or (not (plist-get test :status)) (not (integerp (plist-get test :status)))))
        (invalid-output (not (plist-get test :output))))
    (when invalid-name
      (message "Error: Could not parse test at %s: no test name" org-filepath))
    (when invalid-source
      (message "Error: Could not parse test at %s: no test source" org-filepath))
    (when invalid-status
      (message "Error: Could not parse test at %s: no expected status" org-filepath))
    (when invalid-output
      (message "Error: Could not parse test at %s: no expected output" org-filepath))
    (if (or
         invalid-name
         invalid-source
         invalid-status
         invalid-output)
        nil
      test)))

(defun run-test--glint-tests ()
  ""
  (message "Running Glint Tests...")
  ;; For every .org file in corpus/glint/ directory, parse and run test.
  (mapc
   (lambda (org-filepath)
     (let ((test (run-test--parse-test-from-org org-filepath))
           (test-source-filepath (make-temp-file (expand-file-name "glinttest") nil ".g")))
       (if (not test)
           (message "Error: Failed to parse test from org file %s" org-filepath)
         (progn
           ;; Write test source to a temporary file, so that LCC can parse from it.
           (with-current-buffer (find-file test-source-filepath)
             (insert (plist-get test :source))
             ;; Actually write the file to disk.
             (save-buffer)
             ;; Remove buffer from Emacs' buffer list. This is mainly so that, if you
             ;; run this script from within a running Emacs, your buffer list doesn't
             ;; get crowded with temporary files that have already been deleted.
             (kill-buffer))

           ;; Run the test
           (run-test--file
            "glint"
            (plist-get test :name)
            test-source-filepath
            (plist-get test :status)
            (plist-get test :output))

           ;; Once the test has finished running, delete the generated test source.
           (delete-file test-source-filepath)))))
   (directory-files "corpus/glint/" t "\\.org\\'"))
  (message "Ran %s Glint Tests: %s Passed"
           (length run-test--completed-list)
           (length run-test--passed-list)))

(defun run-test--main ()
  ""
  ;; Initialize state
  (setq run-test--completed-list ())
  (setq run-test--passed-list ())

  ;; Turn off file backups (becomes a mess with all of the I/O that tests
  ;; require)
  (let ((backup-inhibited t))
    (run-test--glint-tests)
    (message "Ran %s Total Tests: %s Passed"
             (length run-test--completed-list)
             (length run-test--passed-list))))

(run-test--main)

(provide 'runtest)

;;; runtest.el ends here
