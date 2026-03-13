;;; runtest.el --- Run programs compiled with LCC, asserting their output and status -*- lexical-binding: t -*-

;; Author: Lens_r
;; Maintainer: Lens_r
;; Version: 0.1.0
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

;;; Commentary:

;;; Code:

;; For parsing test declarations written in Org-mode.
(require 'org-element)

(defcustom
  lcc-path "/home/lens_r/Programming/play/LensorCompilerCollection/dbg/lcc"
  "The path to the LCC compiler on your system")

(defface run-test--face-passed
  '((t :foreground "green"))
  "The face to propertize \"PASSED\" with in output.")

(defface run-test--face-failed
  '((t :foreground "red"))
  "The face to propertize \"FAILED\" with in output.")

(defvar run-test--completed-list ()
  "List of completed tests")

(defvar run-test--passed-list ()
  "List of completed tests that passed their assertions")

(defun run-test--print-test-result (test)
  "Print PASSED or FAILED, basically"
  (unless (plistp test)
    (error "Expected test to be a plist"))
  (message "%s: %s"
           (if (plist-get test :failed)
               (propertize "FAILED" 'face 'run-test--face-failed)
             (propertize "PASSED" 'face 'run-test--face-passed))
           (plist-get test :name)))

(defun run-test--completed (test-name)
  (unless (stringp test-name)
    (error "Expected 'test-name' to be a string"))
  (memq test-name run-test--completed-list))

(defun run-test--mark-completed (test-name)
  (unless (stringp test-name)
    (error "Expected 'test-name' to be a string"))
  (push test-name run-test--completed-list))

(defun run-test--mark-passed (test-name)
  (unless (stringp test-name)
    (error "Expected 'test-name' to be a string"))
  (push test-name run-test--passed-list))

(defun run-test--process-status-unexpected-p
    (test-name p-status event)
  "Return 'nil' iff 'p-status' is equal to the symbol 'exit'.
'p-status' is expected to be a return value from 'process-status'."
  (unless (eq p-status 'exit)
    (message "UNEXPECTED EXIT: %s\n\t%s"
             test-name event)))

;; Compare program exit status to expected program exit status.
(defun run-test--status-unexpected-p
    (test-name expected-status got-status)
  "Return 'nil' iff 'got-status' is equal to 'expected-status' (using '=')"
  (unless (= expected-status got-status)
    (message "UNEXPECTED STATUS: %s\n\tEXPECTED %i\n\tGOT %i"
             test-name expected-status got-status)))

;; Compare program output to expected program output.
(defun run-test--output-unexpected-p
    (test-name expected-output got-output)
  "Return 'nil' iff 'got-output' conforms to restrictions by 'expected-output'.
'expected-output' may either be a string (exact match using 'string-equal'),
or a function that will be applied using funcall with a single argument
of the output received.
Otherwise, return a human-readable string that details the difference
in the expected output and what we got."

  ;; TODO: Better visualisation of what's different, invisible characters, etc.
  (cond
   ((stringp expected-output)
    (unless (string-equal got-output expected-output)
      ;; TODO: Get amount of characters that match from beginning using compare-strings...
      ;; Propertize substring after matching characters to be red in GOT output...
      (message
       "UNEXPECTED OUTPUT: %s\n\tEXPECTED:\n'%s'\n\tGOT:\n'%s'\n%s\n%s"
       test-name
       expected-output got-output
       (string-to-list expected-output) (string-to-list got-output))))
   ((functionp expected-output)
    (unless (funcall expected-output got-output)
      (message
       "UNEXPECTED OUTPUT: %s\n\tEXPECTED FUNCTION MATCHER TO RETURN t, BUT GOT nil INSTEAD\n\tmatcher: %S\n\tGOT: %s"
       test-name expected-output got-output)))
   (t (error "Unhandled expected-output type..."))))

(defun run-test--wait-for-test-completion (test)
  "Sit until a test is registered as completed"
  (while
      (and
       (not (plist-get test :failed))
       (not (run-test--completed (plist-get test :name))))
    (sit-for 0.01)))

(defun run-test--parse-test-from-org (org-filepath)
  "Returns a property list containing information about the parsed test, or nil
 if parsing did not succeed.
Properties:
:name    Human-readable name
:source  Input source code
:status  Expected status from compiled+executed test
:output  Expected output from compiled+executed test

Additional properties are included, but not necessary:
:path    Path to org-file containing test declaration
:flags   List of flags to pass to LCC when compiling the test
"
  (when (not (file-name-absolute-p org-filepath))
    (error "Expected absolute filepath to parse org file from..."))

  (let ((test `(:name
                ""
                :source ,nil
                :status ,nil :output ,nil
                :path ,org-filepath
                :flags ,nil
                :artifacts ,nil
                :intermediate_artifacts ,nil)))
    (with-current-buffer (find-file-noselect org-filepath)
      (let ((parsetree (org-element-parse-buffer)))
        (org-element-map
            parsetree
            t
          (lambda (elem)
            ;; (message "%s" (org-element-interpret-data elem))
            ;; (message "Contents:-->%s<--" elem)
            (when (eq (org-element-type elem) 'headline)
              (setf test
                    (plist-put
                     test :name (org-element-interpret-data (org-element-property :title elem)))))
            (when (string-equal (org-element-property :name elem) "source")
              (push (org-element-property :value elem) (plist-get test :source)))
            (when (string-equal (org-element-property :name elem) "status")
              (setf test
                    (plist-put
                     test :status (string-to-number (org-element-property :value elem)))))
            (when (string-equal (org-element-property :name elem) "output")
              (if (eq (org-element-type elem) 'src-block)
                  (progn
                    (when (string-empty-p (org-element-property :value elem))
                      (error (concat "Source code block named 'output' is expected to define a matcher function."
                                     " You probably meant to make an empty /example/ block.")))
                    (setf test
                          (plist-put
                           test :output (read (org-element-property :value elem))))
                    (unless (functionp (plist-get test :output))
                      (error "Source code block named 'output' is expected to define a matcher function, but got %s instead."
                             (symbol-name (type-of (plist-get test :output))))))
                (progn
                  (unless (eq (org-element-type elem) 'example-block)
                    (error (concat
                            "Named output block is either an example block containing exact output to match,"
                            " or a source block with Emacs Lisp inside, defining a matcher function.")))
                  (setf test
                        (plist-put
                         test :output (org-element-property :value elem))))))
            (when (string-equal (org-element-property :name elem) "flags")
              (mapc
               (lambda (flag)
                 (push flag (plist-get test :flags)))
               (split-string (org-element-property :value elem))))
            ))))

    ;; Reverse collected flags (if any)
    (setf (plist-get test :flags)
          (reverse (plist-get test :flags)))

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
        test))))

(defun run-test--sources-to-files (sources)
  "Given a list of strings, write each string to a separate file,
returning a list of strings representing file names where the input
strings were stored to."
  (unless (listp sources)
    (error "Expected list of strings representing a test's sources"))
  (mapcar
   (lambda (source)
     (make-temp-file (expand-file-name "glinttest") nil ".g" source))
   sources))

(defun run-test--glint-tests ()
  "Run each of the tests in the Glint corpus"
  (message "Running Glint Tests...")

  ;; For every .org file in corpus/glint/ directory, parse and run the test.
  ;; NOTE: If we used a child emacs process to operate on the parsed test,
  ;; we could dispatch each test as soon as it's parsed, running them in
  ;; parallel. We may even be able to parse tests in parallel.
  (mapc
   (lambda (org-filepath)
     (let ((test (run-test--parse-test-from-org org-filepath)))
       (if (not test)
           (message
            "Error: Failed to parse test from org file %s"
            org-filepath)
         (let
             ((source-files
               (run-test--sources-to-files (plist-get test :source)))
              (output-file
               (make-temp-file (expand-file-name "glinttest")))
              (program-output ""))
           ;; Run LCC
           (make-process
            :name "lcc"
            :buffer nil
            :command
            `(,lcc-path
              ,@source-files
              "-x" "glint"
              "-o" ,output-file
              "--run")
            :filter
            (lambda (p o) (setf program-output (concat program-output o)))
            :sentinel
            (lambda (p e)
              ;; (message "Process: %s had the event '%s' (process-status:%s)" p e (process-status p))
              (when (memq (process-status p) '(exit signal))
                ;; Set failure flag if status or output is unexpected.
                ;; Record test as passing if test passed.
                (if (or
                     (run-test--process-status-unexpected-p
                      (plist-get test :name) (process-status p) e)
                     (run-test--output-unexpected-p
                      (plist-get test :name) (plist-get test :output) program-output)
                     (run-test--status-unexpected-p
                      (plist-get test :name) (plist-get test :status) (process-exit-status p)))
                    (setf test (plist-put test :failed t))
                  (run-test--mark-passed (plist-get test :name)))

                ;; Mark test as completed
                (run-test--mark-completed (plist-get test :name)))))

           (run-test--wait-for-test-completion test)
           (run-test--print-test-result test)

           ;; TODO: GMeta files

           ;; Once the program has exited, we no longer need it's executable...
           ;; (message "Test %s: Deleting executable file %s" (plist-get test :name) output-file)
           (delete-file output-file)

           ;; Delete intermediate assembly files
           (mapc
            (lambda (source-path)
              (delete-file (file-name-with-extension source-path ".s")))
            source-files)

           ;; Delete source files
           (mapc #'delete-file source-files)))))
   (directory-files "corpus/glint/" 'absolute "\\.org\\'"))

  ;; Hacky fix: I'm not exactly sure where these are coming from, but I can't
  ;; seem to get all of the executables to delete...
  (mapc #'delete-file
        (directory-files "." 'absolute "\\glinttest.*\\'"))

  ;; Once tests are over, we should probably clean up any .gmeta files that
  ;; we may have created over the course of compiling the tests.
  (mapc #'delete-file
        (directory-files "." 'absolute "\\.gmeta\\'"))

  ;; Print rundown of Glint tests (what happened)
  (message "Ran %s Glint Tests: %s Passed"
           (length run-test--completed-list)
           (length run-test--passed-list)))

(defun run-test--ir-tests ()
  ""
  (message "Running LCC IR Tests...")
  (defvar test '(error "NOT A GLOBAL"))

  (mapc
   (lambda (org-filepath)
     (let ((test (run-test--parse-test-from-org org-filepath)))
       (if (not test)
           (message
            "Error: Failed to parse test from org file %s"
            org-filepath)
         (let
             ((source-files
               (run-test--sources-to-files (plist-get test :source)))
              (output-file
               (make-temp-file (expand-file-name "irtest")))
              (program-output ""))
           ;; Run LCC
           (make-process
            :name "lcc"
            :buffer nil
            :command
            `(,lcc-path
              ,@source-files
              "-x" "ir"
              "-o" ,output-file
              "--run")
            :filter
            (lambda (p o) (setf program-output (concat program-output o)))
            :sentinel
            (lambda (p e)
              ;; (message "Process: %s had the event '%s' (process-status:%s)" p e (process-status p))
              (when (memq (process-status p) '(exit signal))
                ;; Once the program has exited, we no longer need it's executable...
                ;; (message "Test %s: Deleting executable file %s" (plist-get test :name) output-file)
                (delete-file output-file)

                ;; Set failure flag if status or output is unexpected.
                ;; Record test as passing if test passed.
                (if (or
                     (run-test--process-status-unexpected-p
                      (plist-get test :name) (process-status p) e)
                     (run-test--output-unexpected-p
                      (plist-get test :name) (plist-get test :output) program-output)
                     (run-test--status-unexpected-p
                      (plist-get test :name) (plist-get test :status) (process-exit-status p)))
                    (setf test (plist-put test :failed t))
                  (run-test--mark-passed (plist-get test :name)))

                ;; Mark test as completed
                (run-test--mark-completed (plist-get test :name)))))

           (run-test--wait-for-test-completion test)
           (run-test--print-test-result test)

           ;; Delete intermediate assembly files
           (mapc
            (lambda (source-path)
              (delete-file (file-name-with-extension source-path ".s")))
            source-files)

           ;; Delete source files
           (mapc #'delete-file source-files)))))
   (directory-files "corpus/ir/" t "\\.org\\'"))

  ;; Print rundown of LCC IR tests (what happened)
  (message "Ran %s LCC IR Tests: %s Passed"
           (length run-test--completed-list)
           (length run-test--passed-list)))

(defun run-test--main ()
  ""
  ;; Initialize state
  (setf run-test--completed-list ())
  (setf run-test--passed-list ())

  ;; Turn off file backups (becomes a mess with all of the I/O that tests
  ;; require)
  (let ((backup-inhibited t))
    ;; Run the tests...
    (run-test--ir-tests)
    (run-test--glint-tests)
    ;; Print test overview (what happened)
    (message "Ran %s Total Tests: %s Passed"
             (length run-test--completed-list)
             (length run-test--passed-list))))

(run-test--main)

(provide 'runtest)

;;; runtest.el ends here
