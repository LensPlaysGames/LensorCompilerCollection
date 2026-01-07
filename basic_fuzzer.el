(defvar fuzz-lcc-path "~/Programming/play/LensorCompilerCollection/dbg/lcc")

(defun fuzzy-random-ascii (length)
  "Return a string of given length containing random characters from the
  ASCII character set (0-127)."
  (if (> length 0)
      (concat
       (list (random 127))
       (fuzzy-random-ascii (1- length)))
    ""))

(fuzzy-random-ascii 8)

(defun build (source failures-ref)
  ;; TODO
  ;; Write "source" string to temporary buffer.
  ;; Save buffer containing source to a temporary file.
  ;; TODO: Unique path
  (with-temp-file "fuzz.g"
    (insert source))
  ;; Pass path to temporary file as argument to lcc.
  ;; Keep track of how the process exits.
  ;; Non-standard exit, non-zero exit code, etc.
  (let
      ((rc (call-process fuzz-lcc-path nil nil nil "fuzz.g")))
    (unless (and (numberp rc) (< rc 128))
      (push (list source (string-to-list source)) failures-ref)
      (message
       (concat
        "!!!FAILED FUZZ!!!\n"
        "Source: %s\n")
       (string-to-list source)))))

;; (build (fuzzy-random-ascii 8))

(defun fuzz-a-lot (how-many)
  (let ((failures nil))
    ;; Run fuzzes, collecting failures
    (dotimes (n how-many)
      (build (fuzzy-random-ascii 64) failures))
    ;; Emit failures to user
    (mapc (lambda (failure) (message "FAILURE:\n%s\n" failure))
          failures)))

;; (fuzz-a-lot 1024)
