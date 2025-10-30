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

;;; Commentary:

;; We use macros and gensym together to create global LISP variables that
;; act as if they are local to the function. Any identifier that is meant
;; to be gensym'd should be defvar'd with an initial value of '(error),
;; such that usage of the un-gensym'd symbol immediately errors.

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
  (unless (plistp test) (error "Expected test to be a plist"))
  (message "%s: %s"
           (if (plist-get test :failed)
               (propertize "FAILED" 'face 'run-test--face-failed)
             (propertize "PASSED" 'face 'run-test--face-passed))
           (plist-get test :name)))

(defun run-test--completed (test-name)
  (unless (stringp test-name) (error "Expected 'test-name' to be a string"))
  (memq test-name run-test--completed-list))

(defun run-test--mark-completed (test-name)
  (unless (stringp test-name) (error "Expected 'test-name' to be a string"))
  (push test-name run-test--completed-list))

(defun run-test--mark-passed (test-name)
  (unless (stringp test-name) (error "Expected 'test-name' to be a string"))
  (push test-name run-test--passed-list))

;; Compare program exit status to expected program exit status.
(defun run-test--status-unexpected-p (test-name expected-status got-status)
  "Return 'nil' iff 'got-status' is equal to 'expected-status' (using '=')"
  (unless (= expected-status got-status)
    (message "UNEXPECTED STATUS: %s\n\tEXPECTED %i\n\tGOT %i"
             test-name expected-status got-status)))

;; Compare program output to expected program output.
(defun run-test--output-unexpected-p (test-name expected-output got-output)
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
      (message "UNEXPECTED OUTPUT: %s\n\tEXPECTED:\n'%s'\n\tGOT:\n'%s'\n%s\n%s"
               test-name expected-output got-output (string-to-list expected-output) (string-to-list got-output))))
   ((functionp expected-output)
    (unless (funcall expected-output got-output)
      (message "UNEXPECTED OUTPUT: %s\n\tEXPECTED FUNCTION MATCHER TO RETURN t, BUT GOT nil INSTEAD\n\tmatcher: %S\n\tGOT: %s"
               test-name expected-output got-output)))
   (t (error "Unhandled expected-output type..."))))

(defmacro run-test--execute-macro (executable-filepath test-name expected-status expected-output)
  (let ((exe-output (gensym "exe-output")))
    `(progn
       (setq ,exe-output "")
       (make-process
        :name "exe"
        :buffer nil
        :command (list executable-filepath)
        :filter
        (lambda (p o)
          (setq ,exe-output (concat ,exe-output o)))
        :sentinel
        (lambda (p e)
          (when (eq 'exit (process-status p))
            ;; Once the program has exited, we no longer need it's executable...
            ;; (message "Test %s: Deleting executable file %s" test-name executable-filepath)
            (delete-file executable-filepath)

            ;; Set failure flag if status or output is unexpected.
            ;; Record test as passing if test passed.
            (if (or
                 (run-test--status-unexpected-p test-name expected-status (process-exit-status p))
                 (run-test--output-unexpected-p test-name expected-output ,exe-output))
                (setq test (plist-put test :failed t))
              (run-test--mark-passed test-name))

            ;; Mark test as completed
            (run-test--mark-completed test-name)))))))

(defun run-test--execute (executable-filepath test-name expected-status expected-output)
  "Run executable"
  (defvar exe-output '(error "You forgot to unquote 'exe-output', most likely"))
  (run-test--execute-macro executable-filepath test-name expected-status expected-output))

(defun run-test--invoke-gcc (source-filepaths output-filepath filter sentinel &optional gcc-args)
  ;; Argument Checking
  (unless (listp source-filepaths)
    (error "source-filepaths invalid"))
  (unless (listp gcc-args)
    (error "gcc-args invalid"))
  ;; Ensure each source filepath is valid and exists.
  (mapc
   (lambda (source-filepath)
     (when (or
            (not (stringp source-filepath))
            (string-empty-p source-filepath)
            (not (file-exists-p source-filepath)))
       (error "source-filepath %s invalid | stringp:%s, empty:%s, exists:%s"
              source-filepath (stringp source-filepath) (string-empty-p source-filepath) (file-exists-p source-filepath))))
   source-filepaths)
  ;; Ensure output file path is valid
  (when (or (not (stringp output-filepath)) (string-empty-p output-filepath))
    (error "output-filepath invalid"))

  (unless (functionp filter)
    (error "'filter' must be a lambda accepting two arguments"))
  (unless (functionp sentinel)
    (error "'sentinel' must be a lambda accepting two arguments"))

  ;; (message
  ;;  "Invoking GCC like: %s"
  ;;  `("gcc" ,@gcc-args ,@source-filepaths "-o" ,output-filepath))

  ;; Actually call gcc
  (make-process
   :name "gcc"
   :buffer nil
   :command `("gcc" ,@gcc-args ,@source-filepaths "-o" ,output-filepath)
   :filter filter
   :sentinel sentinel))

(defmacro run-test--gcc-macro (source-filepaths test-name expected-status expected-output)
  (let ((gcc-output (gensym "gcc-output"))
        (gcc-output-filepath (gensym "gcc-output-filepath")))
    `(progn
       (setq ,gcc-output ""
             ,gcc-output-filepath (file-name-with-extension (make-temp-name (expand-file-name "a")) "out"))
       (run-test--invoke-gcc
        source-filepaths ,gcc-output-filepath
        (lambda (p o)
          (setq ,gcc-output (concat ,gcc-output o)))
        (lambda (p e)
          (when (eq 'exit (process-status p))
            ;; Delete artifacts, once they are no longer needed.
            ;; (mapc #'delete-file source-filepaths);

            ;; Upon success, execute the test.
            ;; Upon failure, mark the test as having failed, and emit an error.
            (if (= (process-exit-status p) 0)
                (run-test--execute ,gcc-output-filepath test-name expected-status expected-output)
              (progn
                (setq test (plist-put test :failed t))
                (message "Error: Test %s: GCC returned non-zero exit code (%i)\n%s"
                         (plist-get test :name) (process-exit-status p) ,gcc-output)))))))))

(defun run-test--gcc (source-filepaths test-name expected-status expected-output)
  "Run GCC"
  (unless (listp source-filepaths)
    (error "Wrong type argument: expected list of sources"))

  (defvar gcc-output-filepath '(error "You forgot to unquote 'gcc-output-filepath', most likely"))
  (defvar gcc-output '(error "You forgot to unquote 'gcc-output', most likely"))
  (run-test--gcc-macro source-filepaths test-name expected-status expected-output))

(defun run-test--invoke-lcc (language source-filepath output-filepath filter sentinel &optional lcc-args)
  (when (or
         (not (stringp source-filepath))
         (string-empty-p source-filepath)
         (not (file-exists-p source-filepath)))
    (error "source-filepath invalid"))
  (when (or (not (stringp output-filepath)) (string-empty-p output-filepath))
    (error "output-filepath invalid"))
  (when (not (functionp filter))
    (error "'filter' must be a lambda accepting two arguments"))
  (when (not (functionp sentinel))
    (error "'sentinel' must be a lambda accepting two arguments"))

  ;; (message "Invoking LCC like: %s" (list lcc-path source-filepath "-x" language "-o" output-filepath))

  (make-process
   :name "lcc"
   :buffer nil
   :command `(,lcc-path ,@lcc-args ,source-filepath "-x" ,language "-o" ,output-filepath)
   :filter filter
   :sentinel sentinel))

(defun run-test--wait-for-test-artifacts (test)
  "Sit until a test has all artifacts compiled."
  (while (and (not (plist-get test :failed)) (not (= (length (plist-get test :artifacts)) (length (plist-get test :source)))))
    (sit-for 0.01)))

(defun run-test--wait-for-test-artifacts-except1 (test)
  "Sit until a test has all artifacts compiled."
  (while (and (not (plist-get test :failed)) (not (= (length (plist-get test :artifacts)) (1- (length (plist-get test :source))))))
    (sit-for 0.01)))

(defun run-test--wait-for-test-completion (test)
  "Sit until a test is registered as completed"
  (while (and (not (plist-get test :failed)) (not (run-test--completed (plist-get test :name))))
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
:path    Path to org-file containing test declaration"
  (when (not (file-name-absolute-p org-filepath))
    (error "Expected absolute filepath to parse org file from..."))

  (let ((test `(:name
                ""
                :source ,nil :status ,nil :output ,nil
                :path ,org-filepath
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
              (setq test
                    (plist-put
                     test :name (org-element-interpret-data (org-element-property :title elem)))))
            (when (string-equal (org-element-property :name elem) "source")
              (push (org-element-property :value elem) (plist-get test :source)))
            (when (string-equal (org-element-property :name elem) "status")
              (setq test
                    (plist-put
                     test :status (string-to-number (org-element-property :value elem)))))
            (when (string-equal (org-element-property :name elem) "output")
              (if (eq (org-element-type elem) 'src-block)
                  (progn
                    (when (string-empty-p (org-element-property :value elem))
                      (error (concat "Source code block named 'output' is expected to define a matcher function."
                                     " You probably meant to make an empty /example/ block.")))
                    (setq test
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
                  (setq test
                        (plist-put
                         test :output (org-element-property :value elem))))))))))
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

(defun run-test--glint-link (test)
  ""
  (run-test--gcc
   (plist-get test :artifacts)
   (plist-get test :name)
   (plist-get test :status)
   (plist-get test :output)))

(defmacro run-test--glint-gcc-macro (test source-filepath)
  (let ((gcc-output (gensym "gcc-output"))
        (glint-gcc-output-filepath (gensym "glint-gcc-output-filepath")))
    `(progn
       (setq ,gcc-output ""
             ,glint-gcc-output-filepath (file-name-with-extension (make-temp-name (expand-file-name "./gcctmp")) "o"))
       (run-test--invoke-gcc
        (list source-filepath) ,glint-gcc-output-filepath
        (lambda (p o) (setq ,gcc-output (concat ,gcc-output o)))
        (lambda (p e)
          (when (eq 'exit (process-status p))
            ;; Once the test has finished compiling, delete the intermediate test
            ;; artifact that LCC generated and GCC used as input.
            ;; (message "Test %s: Deleting file %s" (plist-get test :name) source-filepath)
            ;; (delete-file source-filepath)

            ;; Upon success, record artifact in test.
            ;; Upon failure, mark test as having failed and emit an error.
            (if (= 0 (process-exit-status p))
                (progn
                  ;; (message "Test %s: GCC generated final artifact %s" (plist-get test :name) ,glint-gcc-output-filepath)
                  ;; Wait for file to exist
                  (while (not (file-exists-p ,glint-gcc-output-filepath))
                    (message "Test %s: Waiting for %s to exist..."
                             (plist-get test :name) ,glint-gcc-output-filepath)
                    (sit-for 0.1))
                  (push ,glint-gcc-output-filepath (plist-get test :artifacts)))
              (progn
                (setq test (plist-put test :failed t))
                (message "Error: Test %s: GCC returned non-zero exit code (%i)\n%s"
                         (plist-get test :name) (process-exit-status p) ,gcc-output)))))
        ;; Tell GCC to create an object file
        '("-c")))))

(defun run-test--glint-gcc (test source-filepath)
  ""
  (defvar gcc-output '(error "You forgot to unquote 'gcc-output', most likely."))
  (defvar glint-gcc-output-filepath '(error "You forgot to unquote 'glint-gcc-output-filepath', most likely."))
  (defvar test-source-filepath '(error "You forgot to unquote 'test-source-filepath', most likely"))
  (run-test--glint-gcc-macro test source-filepath))

(defmacro run-test--glint-lcc-macro (test test-source)
  ;; NOTE: Exactly like cl-with-gensyms
  (let ((glint-lcc-output (gensym "glint-lcc-output"))
        (glint-lcc-output-filepath (gensym "glint-lcc-output-filepath"))
        (test-source-filepath (gensym "test-source-filepath")))
    ;; We use 'set' so that the symbol will be evaluated to the generated symbol
    `(progn
       (setq ,glint-lcc-output ""
             ,glint-lcc-output-filepath (file-name-with-extension (make-temp-name (expand-file-name "./lcctmp")) "s")
             ,test-source-filepath (make-temp-file (expand-file-name "glinttest") nil ".g"))

       ;; Generate unique file to write test source into.
       (with-current-buffer (find-file ,test-source-filepath)
         (insert test-source)
         (save-buffer)
         (kill-buffer))

       ;; Compile test source using LCC
       (run-test--invoke-lcc
        "glint" ,test-source-filepath ,glint-lcc-output-filepath
        ;; Collect program output using filter function
        (lambda (p o) (setq ,glint-lcc-output (concat ,glint-lcc-output o)))
        ;; Run this sentinel function when program exits.
        (lambda (p e)
          (when (eq 'exit (process-status p))
            ;; Once the test has finished running, delete the generated test source.
            ;; (message "Test %s: Deleting intermediate file %s" (plist-get test :name) ,test-source-filepath)
            (delete-file ,test-source-filepath)
            ;; Upon success, compile generated assembly into an object file using gcc.
            ;; Upon failure, mark the test as having failed, and emit an error.
            (if (= 0 (process-exit-status p))
                (progn
                  ;; (message "Test %s: LCC generated intermediate artifact %s" (plist-get test :name) ,glint-lcc-output-filepath)
                  ;; wait for file to exist
                  (while (not (file-exists-p ,glint-lcc-output-filepath))
                    (message "Test %s: Waiting for %s to exist..."
                             (plist-get test :name) ,glint-lcc-output-filepath)
                    (sit-for 0.1))
                  (push ,glint-lcc-output-filepath (plist-get test :intermediate_artifacts))
                  (run-test--glint-gcc test ,glint-lcc-output-filepath))
              (progn
                (setq test (plist-put test :failed t))
                (message "Error: Test %s: LCC returned non-zero exit code (%i)\n%s"
                         (plist-get test :name) (process-exit-status p) ,glint-lcc-output)))))
        '("-I" ".")))))

(defun run-test--glint-lcc (test test-source)
  "Compile a Glint test source into an object file using LCC, then GCC."
  (unless (stringp test-source)
    (error "Expected 'test-source' to be a string"))
  ;; If a test has multiple sources, this ensures we don't continue trying
  ;; to compile sources of a test that has already had a source fail to
  ;; compile...
  ;; NOTE: These have to be defvar'd and not let'd because they are used
  ;; within the sentinel, and the sentinel runs after we have exited the
  ;; scope of the let, and so all the bindings are no longer valid.
  (defvar glint-lcc-output '(error "You forgot to unquote 'glint-lcc-output', most likely"))
  (defvar glint-lcc-output-filepath '(error "You forgot to unquote 'glint-lcc-output-filepath', most likely"))
  (run-test--glint-lcc-macro test test-source))

(defun run-test--glint-tests ()
  ""
  (message "Running Glint Tests...")
  (defvar test '(error "NOT A GLOBAL"))

  ;; For every .org file in corpus/glint/ directory, parse and run the test.
  ;; NOTE: If we used a child emacs process to operate on the parsed test,
  ;; we could dispatch each test as soon as it's parsed, running them in
  ;; parallel. We may even be able to parse tests in parallel.
  (mapc
   (lambda (org-filepath)
     (let ((test (run-test--parse-test-from-org org-filepath)))
       (if (not test)
           (message "Error: Failed to parse test from org file %s" org-filepath)
         (progn
           ;; Begin compiling each non-executable (non-last) source.
           (mapc
            (lambda (source) (run-test--glint-lcc test source))
            (butlast (plist-get test :source)))

           ;; Wait for non-executable test artifacts to complete compilation...
           (run-test--wait-for-test-artifacts-except1 test)

           (if (plist-get test :failed)
               (progn
                 ;; Test is finished processing.
                 (run-test--mark-completed (plist-get test :name))
                 (mapc #'delete-file (plist-get test :intermediate_artifacts))
                 (mapc #'delete-file (plist-get test :artifacts))
                 (message "Error: Test %s failed to compile non-executable artifacts" (plist-get test :name))
                 (run-test--print-test-result test))
             (progn
               ;; Compile executable source
               (run-test--glint-lcc test (car (last (plist-get test :source))))

               ;; Wait for executable artifact to complete compilation
               (run-test--wait-for-test-artifacts test)

               ;; Delete all intermediate artifacts...
               ;; Basically, LCC generates assembly that isn't included in the final
               ;; link, so we can get rid of it here.
               (mapc #'delete-file (plist-get test :intermediate_artifacts))

               (if (plist-get test :failed)
                   (progn
                     ;; Test is finished processing.
                     (run-test--mark-completed (plist-get test :name))
                     (mapc #'delete-file (plist-get test :artifacts))
                     (message "Error: Test %s failed to compile to object file" (plist-get test :name))
                     (run-test--print-test-result test))
                 (progn
                   ;; Validate that all artifacts exist
                   (mapc
                    (lambda (a)
                      (unless (file-exists-p a)
                        (error "Test Artifact %s Doesn't Exist!" a)))
                    (plist-get test :artifacts))

                   ;; Begin linking artifacts into an executable...
                   (run-test--glint-link test)

                   ;; Wait for test completion
                   (run-test--wait-for-test-completion test)

                   (run-test--print-test-result test)

                   ;; Delete test artifacts once they are no longer needed.
                   (mapc #'delete-file (plist-get test :artifacts))))))))))
   (directory-files "corpus/glint/" t "\\.org\\'"))

  ;; Once tests are over, we should probably clean up any .gmeta files that
  ;; we may have created over the course of compiling the tests.

  ;; Print rundown of Glint tests (what happened)
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
    ;; Run the tests...
    (run-test--glint-tests)
    ;; Print test overview (what happened)
    (message "Ran %s Total Tests: %s Passed"
             (length run-test--completed-list)
             (length run-test--passed-list))))

(run-test--main)

(provide 'runtest)

;;; runtest.el ends here
