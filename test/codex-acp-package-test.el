;;; codex-acp-package-test.el --- Adapter dependency boundary -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'ert)
(require 'cl-lib)
(defconst codex-acp-package-test--root
  (expand-file-name "../" (file-name-directory load-file-name)))
(ert-deftest codex-acp-package-loads-without-interactive-file-integration ()
  (with-temp-buffer
    (let ((status
           (apply #'call-process
                  (expand-file-name invocation-name invocation-directory) nil t nil
                  "-Q" "--batch"
                  (append (cl-mapcan (lambda (directory) (list "-L" directory))
                                     (list codex-acp-package-test--root
                                           (file-name-directory (locate-library "acp"))
                                           (file-name-directory (locate-library "async"))))
                          '("--eval" "(progn (require 'codex-acp) (when (or (featurep 'tramp-async-files) (featurep 'dired-aux)) (error \"Interactive integration loaded with adapter\")) (unless (commandp 'tramp-async-files-find-file) (error \"Interactive compatibility entry is missing\")))")))))
      (ert-info ((buffer-string)) (should (zerop status))))))
(ert-deftest codex-acp-package-core-unloads-without-loading-interactive-files ()
  (with-temp-buffer
    (let ((status
           (call-process
            (expand-file-name invocation-name invocation-directory) nil t nil
            "-Q" "--batch" "-L" codex-acp-package-test--root
            "-L" (file-name-directory (locate-library "async"))
            "--eval"
            "(progn (require 'tramp-async) (unload-feature 'tramp-async t) (when (or (featurep 'tramp-async-files) (featurep 'dired-aux)) (error \"Unloading core loaded interactive integration\")))")))
      (ert-info ((buffer-string)) (should (zerop status))))))
(provide 'codex-acp-package-test)
