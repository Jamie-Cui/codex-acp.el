;;; check.el --- Offline package checks -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;; Use installed dependencies, without loading the user's initialization.
;; EXTRA_LOAD_PATH accepts path-separated dependency checkout directories.
;;; Code:
(require 'package)
(when-let* ((directory (getenv "ELPA_DIR")))
  (setq package-user-dir (expand-file-name directory)))
(package-initialize)
(require 'cl-lib)
(require 'bytecomp)
(require 'ert)
(defconst package-check-root
  (expand-file-name "../" (file-name-directory load-file-name)))
;; These packages currently expose their entry libraries at checkout roots.
(dolist (name '(ecss ebox))
  (when-let* ((descriptor (cadr (assq name package-alist))))
    (add-to-list 'load-path (package-desc-dir descriptor))))
(dolist (directory (split-string (or (getenv "EXTRA_LOAD_PATH") "") path-separator t))
  (add-to-list 'load-path (expand-file-name directory)))
(dolist (directory '("tools" "test" "."))
  (add-to-list 'load-path (expand-file-name directory package-check-root)))
(setq load-prefer-newer t)
(defun package-check-files ()
  "Return library sources, excluding generated package files."
  (seq-remove (lambda (file) (string-match-p "-\\(?:autoloads\\|pkg\\)[.]el\\'" file))
              (directory-files package-check-root t "[.]el$")))
(defun package-check-compile ()
  "Byte-compile libraries to a temporary directory."
  (let* ((output (make-temp-file "package-compile-" t))
         (byte-compile-dest-file-function
          (lambda (file) (expand-file-name (concat (file-name-nondirectory file) "c") output)))
         (byte-compile-error-on-warn t))
    (unwind-protect
        (dolist (file (package-check-files))
          (unless (byte-compile-file file) (error "Compilation failed: %s" file)))
      (delete-directory output t))))
(defun package-check-lint ()
  "Check library metadata with package-lint."
  (require 'package-lint)
  (let (failed)
    (dolist (file (package-check-files))
      (with-temp-buffer
        (insert-file-contents file)
        (setq buffer-file-name file)
        (emacs-lisp-mode)
        (when (equal (file-name-nondirectory file) "codex-acp-worker.el")
          (setq-local package-lint-main-file (expand-file-name "codex-acp.el" package-check-root)))
        (when (string-prefix-p "tramp-async-" (file-name-nondirectory file))
          (setq-local package-lint-main-file (expand-file-name "tramp-async.el" package-check-root)))
        (when-let* ((issues (package-lint-buffer)))
          (message "%s: %S" file issues)
          (when (seq-some (lambda (issue) (eq (nth 2 issue) 'error)) issues)
            (setq failed t)))))
    (when failed (error "Package lint reported diagnostics"))))
(defun package-check-test ()
  "Load the checkout's suites once and run them."
  (dolist (file (directory-files (expand-file-name "test" package-check-root) t "-test[.]el$"))
    (unless (featurep (intern (file-name-base file)))
      (load file nil t t)))
  (ert-run-tests-batch-and-exit))
;;; check.el ends here
