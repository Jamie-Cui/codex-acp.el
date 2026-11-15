;;; tramp-async-files.el --- Optional interactive file and Dired integration -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Assisted-by: Codex:GPT-6
;; Maintainer: Jamie Cui <jamie.cui@outlook.com>
;; Version: 0.4.0
;; URL: https://github.com/Jamie-Cui/codex-acp.el
;; Keywords: tools, processes
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of codex-acp.
;;
;; codex-acp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; codex-acp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with codex-acp.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;; Optional interactive file and Dired integration.
;;; Code:
(require 'tramp-async)
(require 'tramp-async-files-worker)

;;; Interactive file visits and saves


(require 'subr-x)
(require 'dired)


(defgroup tramp-async-files nil "Interactive asynchronous remote files." :group 'tramp-async)
(defcustom tramp-async-files-size-limit (* 16 1024 1024)
  "Maximum bytes to visit or save in one asynchronous file snapshot."
  :type 'integer :group 'tramp-async-files)
(defcustom tramp-async-files-auto-save-directory
  (expand-file-name "tramp-async-autosave/" user-emacs-directory)
  "Local directory for recovery files when `auto-save-default' is enabled."
  :type 'directory :group 'tramp-async-files)

(defconst tramp-async-files--save-settings
  '(make-backup-files backup-inhibited backup-by-copying
    backup-by-copying-when-linked backup-by-copying-when-mismatch
    backup-directory-alist tramp-backup-directory-alist version-control
    kept-old-versions kept-new-versions delete-old-versions
    file-precious-flag file-preserve-symlinks-on-save))
(define-error 'tramp-async-files-sync-io "Hook attempted uncached synchronous remote I/O" 'file-error)
(define-error 'tramp-async-files-hook-error "Buffer hook initialization failed")
(define-error 'tramp-async-files-conflict "File changed since visited or saved" 'file-error)
(cl-defstruct (tramp-async-files--state (:constructor tramp-async-files--state-create))
  file snapshot version job phase tick again conflict error)
(defvar-local tramp-async-files--state nil)
(defvar-local tramp-async-files--original-locals nil)
(defvar-local tramp-async-files--had-auto-revert nil)
(defvar tramp-async-file-mode)
(put 'tramp-async-files--state 'permanent-local t)
(put 'tramp-async-files--original-locals 'permanent-local t)
(put 'tramp-async-files--had-auto-revert 'permanent-local t)
(defvar tramp-async-files--guard-snapshot nil)
(defvar tramp-async-files--guard-buffer nil)
(defvar tramp-async-files--installing-buffer nil)
(defvar-local tramp-async-files--format-token nil)
(defvar tramp-async-files--save-followup nil)
(defvar tramp-async-files--save-completion nil)
(defvar apheleia-mode)
(defvar apheleia-format-after-save-in-progress)
(defvar apheleia-post-format-hook)
(defvar dired-omit-mode)
(defvar dired-omit-localp)
(declare-function dired-omit-regexp "dired-x" ())
(declare-function dired-omit-case-fold-p "dired-x" (dir))
(declare-function dired-mark-unmarked-files "dired-x" (regexp msg &optional unflag-p localp case-fold))
(declare-function dired-subtree-insert "dired-subtree" ())
(declare-function apheleia--get-formatters "apheleia-formatters" (&optional interactive))
(declare-function apheleia-format-buffer "apheleia" (formatter &optional success-callback &key callback))
(defvar tramp-async-files--remote-handler nil)
(defvar tramp-async-files-mode)
(defvar envrc-mode)
(defvar envrc--env-dir)
(defvar envrc--running)
(defvar envrc--status)
(defvar envrc-debug)
(defvar envrc-direnv-executable)
(defvar envrc-remote)
(defvar envrc-supported-tramp-methods)
(declare-function envrc--apply "envrc" (buf result))

(defun tramp-async-files--warning (message)
  "Report MESSAGE from a local context, including first warning-buffer setup."
  (let ((default-directory temporary-file-directory))
    (display-warning 'tramp-async-files message)))

(defun tramp-async-files--expand (name &optional directory)
  "Expand NAME against DIRECTORY without resolving remote home directories."
  (let* ((base (or directory default-directory))
         ;; With a live connection, ordinary `file-remote-p' expands names.
         ;; That reenters this function under our guard, and can also resolve
         ;; remote homes outside it.  Only parse names here, even if connected.
         (prefix (or (file-remote-p name nil 'never)
                     (and (not (file-name-quoted-p name)) (file-remote-p base nil 'never)))))
    (if (not prefix) (expand-file-name name base)
      (let* ((remote (file-remote-p name nil 'never))
             (local (if remote (tramp-file-name-localname (tramp-dissect-file-name name 'noexpand)) name))
             (base-local (if (equal (file-remote-p base nil 'never) prefix)
                             (tramp-file-name-localname (tramp-dissect-file-name base 'noexpand)) "/")))
        ;; A remote ~ belongs to the remote account, never the local account.
        (concat prefix
                (if (or (string-prefix-p "~" local)
                        (and (not (file-name-absolute-p local))
                             (string-prefix-p "~" base-local)))
                    (if (or remote (file-name-absolute-p local) (string-prefix-p "~" local))
                        local (concat (file-name-as-directory base-local) local))
                  (let ((file-name-handler-alist nil)) (expand-file-name local base-local))))))))

(defun tramp-async-files--expand-aliases (name directory aliases)
  "Expand NAME in DIRECTORY using only worker-observed directory ALIASES."
  (let ((expanded (tramp-async-files--expand name directory)) best)
    (dolist (entry aliases)
      (when (and (or (equal (directory-file-name expanded) (directory-file-name (car entry)))
                     (string-prefix-p (car entry) expanded))
                 (or (null best) (> (length (car entry)) (length (car best)))))
        (setq best entry)))
    (if (not best) expanded
      (tramp-async-files--expand
       (if (equal (directory-file-name expanded) (directory-file-name (car best)))
           (if (string-suffix-p "/" expanded) (cdr best) (directory-file-name (cdr best)))
         (concat (cdr best) (substring expanded (length (car best)))))))))

(defun tramp-async-files--handler (operation &rest args)
  "Serve observed current-operation metadata, rejecting uncached remote I/O."
  (let* ((snapshot tramp-async-files--guard-snapshot)
         (file (if (and (stringp (car args))
                        (memq operation '(file-attributes file-truename file-exists-p file-readable-p
                                          file-writable-p file-regular-p file-directory-p file-symlink-p
                                          file-modes file-name-case-insensitive-p file-equal-p)))
                   (tramp-async-files--expand-aliases
                    (car args) default-directory (plist-get snapshot :aliases))
                 (car args)))
         (metadata (cdr (assoc file (plist-get snapshot :metadata))))
         (entry (assq operation metadata))
         (other-file (and (eq operation 'file-equal-p)
                          (tramp-async-files--expand-aliases
                           (cadr args) default-directory (plist-get snapshot :aliases))))
         (case-entry
          (and (eq operation 'file-name-case-insensitive-p)
               (or (assoc (file-name-as-directory file) (plist-get snapshot :case-fold))
                   (assoc (file-name-directory file) (plist-get snapshot :case-fold))))))
    (cond
     ((eq operation 'file-remote-p)
      (apply tramp-async-files--remote-handler operation args))
     ((eq operation 'expand-file-name)
      (tramp-async-files--expand-aliases (car args) (cadr args) (plist-get snapshot :aliases)))
     ((memq operation '(lock-file unlock-file)) nil)
     ((eq operation 'abbreviate-file-name) file)
     ((memq operation '(file-name-directory file-name-nondirectory file-name-as-directory
                        directory-file-name substitute-in-file-name))
      (let ((file-name-handler-alist nil)) (apply operation args)))
     ;; String UID/GID attributes require a separately fetched representation.
     ((and entry (not (and (eq operation 'file-attributes) (eq (cadr args) 'string))))
      (cdr entry))
     (case-entry (cdr case-entry))
     ((and (eq operation 'file-equal-p) metadata
           (assoc other-file (plist-get snapshot :metadata)))
      (let ((left (cdr (assq 'identity metadata)))
            (right (cdr (assq 'identity (cdr (assoc other-file (plist-get snapshot :metadata)))))))
        (and left right (equal left right))))
     (t (signal 'tramp-async-files-sync-io (list (symbol-name operation) file))))))

(defun tramp-async-files--io-a (original operation &rest args)
  "Use cached I/O only for the buffer owning this operation; otherwise ORIGINAL."
  (if (eq (current-buffer) tramp-async-files--guard-buffer)
      (let ((tramp-async-files--remote-handler original))
        (apply #'tramp-async-files--handler operation args))
    (apply original operation args)))

(defmacro tramp-async-files--without-remote-io (&rest body)
  "Evaluate BODY against this buffer's observed metadata, without global rebinding."
  (declare (indent 0) (debug t))
  `(let ((tramp-async-files--guard-buffer (current-buffer))
         (tramp-async-files--guard-snapshot
          (and tramp-async-files--state (tramp-async-files--state-snapshot tramp-async-files--state))))
     ,@body))

(defun tramp-async-files--envrc-executable ()
  "Return the executable for automatic remote envrc preparation, or nil."
  (when (and (featurep 'envrc) (bound-and-true-p envrc-global-mode)
             envrc-remote
             (member (file-remote-p default-directory 'method 'never)
                     envrc-supported-tramp-methods))
    envrc-direnv-executable))

(defun tramp-async-files--apply-environment ()
  "Apply the prepared environment before dependent mode hooks in this buffer."
  (when (and (eq (current-buffer) tramp-async-files--installing-buffer)
             (featurep 'envrc) tramp-async-files--state)
    (let ((snapshot (tramp-async-files--state-snapshot tramp-async-files--state)))
      (when (plist-member snapshot :envrc)
        (let ((value (plist-get snapshot :envrc)) (envrc-debug nil))
          (setq-local envrc--env-dir (plist-get value :directory))
          (envrc--apply (current-buffer) (plist-get value :result)))))))

(defun tramp-async-files--envrc-a (original &optional force)
  "Consume prepared data during installation; preserve ORIGINAL's sync contract.
Outside installation, including explicit reloads, ORIGINAL retains ownership."
  (if (and (not force) (eq (current-buffer) tramp-async-files--installing-buffer)
           tramp-async-files--state
           (plist-member (tramp-async-files--state-snapshot tramp-async-files--state) :envrc))
      (tramp-async-files--apply-environment)
    (funcall original force)))

(defmacro tramp-async-files--with-environment (&rest body)
  "Initialize BODY only after its prepared environment is available."
  (declare (indent 0) (debug t))
  `(let ((tramp-async-files--installing-buffer (current-buffer))
         (change-major-mode-after-body-hook
          (cons #'tramp-async-files--apply-environment change-major-mode-after-body-hook)))
     ,@body))

(with-eval-after-load 'envrc
  (when (bound-and-true-p tramp-async-files-mode)
    (advice-add 'envrc--get-current-env-or-run-direnv :around #'tramp-async-files--envrc-a)))

(defun tramp-async-files--run-hook (hook)
  "Run HOOK in order; record failures instead of claiming complete initialization."
  (tramp-async-files--without-remote-io
    (let ((vc-ignore-dir-regexp (concat vc-ignore-dir-regexp "\\|" tramp-file-name-regexp))
          failures)
      (run-hook-wrapped
       hook (lambda (function &rest args)
              (condition-case err (apply function args)
                (error (push (cons function err) failures)))
              nil))
      (when failures
        (let ((description (mapconcat
                            (lambda (entry) (format "%s: %s" (car entry) (error-message-string (cdr entry))))
                            (nreverse failures) "; ")))
          (when tramp-async-files--state
            (setf (tramp-async-files--state-phase tramp-async-files--state) 'error
                  (tramp-async-files--state-error tramp-async-files--state) description))
          (signal 'tramp-async-files-hook-error (list description)))))))

(defun tramp-async-files--status ()
  "Return the current buffer's short asynchronous file status."
  (when tramp-async-files--state
    (pcase (tramp-async-files--state-phase tramp-async-files--state)
      ('opening " Loading…") ('locals " Locals…") ('saving " Saving…") ('preparing " Preparing…") ('mutating " Dired I/O…") ('error " I/O error")
      (_ " Async"))))

(defun tramp-async-files--setup-h ()
  "Keep managed buffers from starting implicit remote lock or revert I/O."
  (when tramp-async-file-mode
    (setq-local create-lockfiles nil)
    ;; C's first-edit path calls lock-file even with create-lockfiles=nil.
    ;; TRAMP's lock handler also checks modtime synchronously.  Defer that
    ;; check to the save worker, without changing other buffers' handlers.
    (setq-local file-name-handler-alist
                (cons '("" . tramp-async-files--lock-handler)
                      (cl-remove #'tramp-async-files--lock-handler file-name-handler-alist :key #'cdr)))
    (setq-local remote-file-name-inhibit-auto-save-visited t)
    (setq-local global-auto-revert-ignore-buffer t)
    (when (bound-and-true-p auto-revert-mode) (auto-revert-mode -1))
    ;; `revert-buffer-function' is a synchronous API, even in a managed buffer.
    ;; Interactive reverts are adapted at the command entry instead.
    (add-hook 'after-revert-hook #'tramp-async-files--sync-metadata-h nil t)
    (add-hook 'kill-buffer-query-functions #'tramp-async-files--kill-query-h nil t)
    (add-hook 'kill-buffer-hook #'tramp-async-files--kill-h nil t)))

(defun tramp-async-files--lock-handler (operation &rest args)
  "Defer this buffer's lock OPERATION and ARGS to worker conflict checking."
  (unless (and tramp-async-file-mode
               (member (car args) (list buffer-file-name buffer-file-truename)))
    (let ((inhibit-file-name-handlers
           (cons #'tramp-async-files--lock-handler
                 (and (eq inhibit-file-name-operation operation) inhibit-file-name-handlers)))
          (inhibit-file-name-operation operation))
      (apply operation args))))
(put 'tramp-async-files--lock-handler 'operations '(lock-file unlock-file))

(defvar tramp-async-file-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap keyboard-quit] #'tramp-async-files-cancel)
    (define-key map [remap recover-this-file] #'tramp-async-files-recover)
    map))
(define-minor-mode tramp-async-file-mode
  "Track asynchronous visits and saves in this buffer."
  :lighter (:eval (tramp-async-files--status)) :keymap tramp-async-file-mode-map
  (if tramp-async-file-mode
      (progn
        (unless tramp-async-files--original-locals
          (setq tramp-async-files--had-auto-revert (bound-and-true-p auto-revert-mode))
          (setq tramp-async-files--original-locals
                (mapcar (lambda (symbol)
                          (list symbol (local-variable-p symbol)
                                (when (boundp symbol) (symbol-value symbol))))
                        '(create-lockfiles file-name-handler-alist remote-file-name-inhibit-auto-save-visited
                          remote-file-name-inhibit-auto-save global-auto-revert-ignore-buffer
                          revert-buffer-function buffer-auto-save-file-name))))
        (add-hook 'after-change-major-mode-hook #'tramp-async-files--setup-h nil t)
        (tramp-async-files--setup-h))
    (tramp-async-files--cancel)
    (remove-hook 'after-change-major-mode-hook #'tramp-async-files--setup-h t)
    (remove-hook 'kill-buffer-query-functions #'tramp-async-files--kill-query-h t)
    (remove-hook 'kill-buffer-hook #'tramp-async-files--kill-h t)
    (remove-hook 'after-revert-hook #'tramp-async-files--sync-metadata-h t)
    (dolist (entry tramp-async-files--original-locals)
      (if (cadr entry) (set (make-local-variable (car entry)) (caddr entry))
        (kill-local-variable (car entry))))
    (setq tramp-async-files--original-locals nil)
    (when tramp-async-files--had-auto-revert (auto-revert-mode 1))
    (setq tramp-async-files--had-auto-revert nil)))
(put 'tramp-async-file-mode 'permanent-local t)
(put 'tramp-async-files--setup-h 'permanent-local-hook t)
(put 'tramp-async-files--kill-query-h 'permanent-local-hook t)
(put 'tramp-async-files--kill-h 'permanent-local-hook t)
(put 'tramp-async-files--sync-metadata-h 'permanent-local-hook t)

(defun tramp-async-files--sync-metadata-h ()
  "Reconcile state after a native synchronous save or revert, without remote I/O."
  (when (and tramp-async-files--state buffer-file-name)
    (setf (tramp-async-files--state-version tramp-async-files--state)
          (list :mtime (visited-file-modtime))
          (tramp-async-files--state-conflict tramp-async-files--state) nil
          (tramp-async-files--state-error tramp-async-files--state) nil
          (tramp-async-files--state-phase tramp-async-files--state) nil)))

(defun tramp-async-files--basic-save-a (original &rest args)
  "Preserve ORIGINAL's synchronous save, then reconcile its acknowledged version."
  (tramp-async-files--pending-a)
  (prog1 (apply original args)
    (when (not (buffer-modified-p)) (tramp-async-files--sync-metadata-h))))

(defun tramp-async-files--cancel ()
  "Cancel this buffer's job while preserving its contents and dirty flag."
  ;; Invalidate only our continuation; Apheleia owns its formatter process.
  (setq tramp-async-files--format-token nil)
  (when-let* ((state tramp-async-files--state) (job (tramp-async-files--state-job state)))
    (when (eq (tramp-async-files--state-phase state) 'saving)
      (set-buffer-modified-p t))
    (setf (tramp-async-files--state-job state) nil
          (tramp-async-files--state-again state) nil
          (tramp-async-files--state-phase state) 'error
          (tramp-async-files--state-error state) "Cancelled; a dispatched mutation may have reached disk")
    (tramp-async-cancel job)
    (force-mode-line-update)))

(defun tramp-async-files-cancel ()
  "Cancel the current file operation, or perform ordinary `keyboard-quit'."
  (interactive)
  (if (or tramp-async-files--format-token
          (and tramp-async-files--state (tramp-async-files--state-job tramp-async-files--state)))
      (progn (tramp-async-files--cancel)
             (message "File operation cancelled; inspect affected files before retrying"))
    (keyboard-quit)))

(defun tramp-async-files--kill-query-h ()
  "Prevent discarding a buffer while its save is still unacknowledged."
  (if (or tramp-async-files--format-token
          (and tramp-async-files--state
               (memq (tramp-async-files--state-phase tramp-async-files--state) '(saving preparing mutating))))
      (progn (message "File operation still pending; wait or use C-g to cancel it first") nil)
    t))
(defun tramp-async-files--kill-h ()
  "Cancel a pending visit when its placeholder buffer is killed."
  (tramp-async-files--cancel))

(defun tramp-async-files--failure (state result)
  "Record and report RESULT's failure in STATE without clearing modifications."
  (let ((err (or (plist-get result :error) '(error "File operation failed"))))
    (when (plist-get result :uncertain) (set-buffer-modified-p t))
    (setf (tramp-async-files--state-phase state) 'error
          (tramp-async-files--state-again state) nil
          (tramp-async-files--state-error state)
          (if (eq (car err) 'tramp-async-files-conflict)
              "File changed since visited or saved" (error-message-string err)))
    (when (eq (car err) 'tramp-async-files-conflict)
      (setf (tramp-async-files--state-conflict state) (list :version (nth 2 err))))
    (tramp-async-files--warning
     (format "%s: %s%s" (tramp-async-files--state-file state)
                             (tramp-async-files--state-error state)
                             (if (plist-get result :uncertain)
                                 "; write outcome unknown. Inspect the target before retrying."
                               (if (tramp-async-files--state-conflict state)
                                   "; revert to inspect, or save again to confirm overwrite." ""))))
    (force-mode-line-update)))

(defun tramp-async-files--metadata (state snapshot)
  "Record acknowledged SNAPSHOT in STATE and the current visiting buffer."
  (unless (plist-member snapshot :locals)
    (setq snapshot (plist-put snapshot :locals
                              (plist-get (tramp-async-files--state-snapshot state) :locals))))
  (setf (tramp-async-files--state-snapshot state) (plist-put (copy-sequence snapshot) :text nil)
        (tramp-async-files--state-version state) (plist-get snapshot :version)
        (tramp-async-files--state-conflict state) nil
        (tramp-async-files--state-error state) nil
        (tramp-async-files--state-phase state) nil)
  (setq buffer-file-truename (plist-get snapshot :path)
        buffer-file-number (when-let* ((attrs (plist-get snapshot :attributes)))
                             (file-attribute-file-identifier attrs)))
  ;; Passing an explicit timestamp needs no stat, even for a remote buffer.
  (let ((file-name-handler-alist nil))
    (set-visited-file-modtime
     (if-let* ((attrs (plist-get snapshot :attributes)))
         (file-attribute-modification-time attrs) 0))))

(defun tramp-async-files--visiting (file)
  "Find FILE's visiting buffer using in-memory names only."
  (cl-find-if
   (lambda (buffer)
     (with-current-buffer buffer
       (or (equal file buffer-file-name) (equal file buffer-file-truename)
           (and tramp-async-files--state
                (eq (tramp-async-files--state-phase tramp-async-files--state) 'opening)
                (equal file (tramp-async-files--state-file tramp-async-files--state)))
           (and tramp-async-files--state
                (plist-get (tramp-async-files--state-snapshot tramp-async-files--state) :directory)
                (equal (directory-file-name
                        (tramp-async-files--expand-aliases
                         file nil (plist-get (tramp-async-files--state-snapshot tramp-async-files--state) :aliases)))
                       (directory-file-name (tramp-async-files--state-file tramp-async-files--state)))))))
   (buffer-list)))

(defun tramp-async-files--auto-save ()
  "Set up a local recovery filename when ordinary auto-saving is enabled."
  (when auto-save-default
    (when (file-remote-p tramp-async-files-auto-save-directory)
      (error "Async recovery directory must be local"))
    (make-directory tramp-async-files-auto-save-directory t)
    (setq-local remote-file-name-inhibit-auto-save nil)
    (setq buffer-auto-save-file-name
          (expand-file-name (concat "#" (secure-hash 'sha256 buffer-file-name) "#")
                            tramp-async-files-auto-save-directory))
    (when (file-exists-p buffer-auto-save-file-name)
      (display-warning 'tramp-async-files
                       (format "%s has a local auto-save; use M-x tramp-async-files-recover before editing"
                               (buffer-name))))
    (set-buffer-auto-saved)))

(defun tramp-async-files-recover ()
  "Restore this buffer's local auto-save copy, keeping it marked modified."
  (interactive)
  (tramp-async-files--pending-a)
  (unless (and buffer-auto-save-file-name
               (not (file-remote-p buffer-auto-save-file-name))
               (file-exists-p buffer-auto-save-file-name))
    (user-error "No local recovery file is available for this buffer"))
  (unless (yes-or-no-p (format "Recover the local auto-save of %s? " (buffer-name)))
    (user-error "Recovery cancelled"))
  (let* ((path buffer-auto-save-file-name)
         (text (with-temp-buffer
                 (when (> (file-attribute-size (file-attributes path)) tramp-async-files-size-limit)
                   (user-error "Recovery file exceeds the asynchronous file size limit"))
                 (insert-file-contents path nil 0 (1+ tramp-async-files-size-limit))
                 (buffer-string)))
        (inhibit-read-only t))
    (save-restriction (widen) (erase-buffer) (insert text))
    (set-buffer-modified-p t)
    (message "Recovered local auto-save; review the buffer before saving")))

(defun tramp-async-files--locals-enabled-p (&optional file)
  "Whether the current buffer permits reading directory variables for FILE."
  (and enable-local-variables enable-dir-local-variables
       (or enable-remote-dir-locals (not (file-remote-p (or file buffer-file-name default-directory))))))

(defun tramp-async-files--locals-a (original &optional predicate)
  "Collect cached directory variables using PREDICATE, or call ORIGINAL.
Native filtering and confirmation still happen in hack-dir-local-variables.
This private Emacs 30 entry also supplies auto-mode-alist before mode setup."
  (if (not (and (eq (current-buffer) tramp-async-files--guard-buffer)
                tramp-async-file-mode tramp-async-files--state
                (plist-member (tramp-async-files--state-snapshot tramp-async-files--state) :locals)))
      (funcall original predicate)
    (when (tramp-async-files--locals-enabled-p)
      (let* ((file (or buffer-file-name default-directory))
             (data (plist-get (tramp-async-files--state-snapshot tramp-async-files--state) :locals)))
        ;; Honor explicit, non-file directory classes installed in this Emacs.
        (dolist (entry dir-locals-directory-cache)
          (when (and (null (nth 2 entry)) (string-prefix-p (car entry) file)
                     (>= (length (car entry)) (length (car data))))
            (setq data (cons (car entry) (dir-locals-get-class-variables (cadr entry))))))
        (when data
          (cons (car data) (dir-locals-collect-variables (cdr data) (car data) nil predicate)))))))

(defun tramp-async-files-refresh-dir-locals ()
  "Reload remote directory variables and rerun normal mode asynchronously."
  (interactive)
  (unless (and tramp-async-file-mode buffer-file-name)
    (user-error "Use this command in an asynchronously visited file"))
  (tramp-async-files--pending-a)
  (let ((buffer (current-buffer)) (state tramp-async-files--state) job)
    (setq job
          (tramp-async-call
           default-directory 'tramp-async-files-worker-context
           (list buffer-file-name tramp-async-files-size-limit
                 (tramp-async-files--locals-enabled-p) (tramp-async-files--envrc-executable))
           (lambda (result)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (eq job (tramp-async-files--state-job state))
                   (setf (tramp-async-files--state-job state) nil)
                   (if (not (eq (plist-get result :status) 'ok))
                       (tramp-async-files--failure state result)
                     (setf (tramp-async-files--state-phase state) nil
                           (tramp-async-files--state-snapshot state)
                           (append (plist-get result :value) (tramp-async-files--state-snapshot state)))
                     (condition-case err
                         (progn
                           (tramp-async-files--with-environment
                             (tramp-async-files--without-remote-io (normal-mode t)))
                           (tramp-async-files--setup-h))
                       ((error quit) (tramp-async-files--failure state (list :error err)))))))))
           :library nil :mutating (and (tramp-async-files--envrc-executable) t)))
    (setf (tramp-async-files--state-job state) job
          (tramp-async-files--state-phase state) 'locals)))

(defun tramp-async-files--normal-mode-a (original &optional find-file)
  "Refresh directory variables for interactive normal-mode; otherwise ORIGINAL."
  (interactive)
  (if (and (called-interactively-p 'any) tramp-async-file-mode buffer-file-name)
      (tramp-async-files-refresh-dir-locals)
    (funcall original find-file)))

(defun tramp-async-files--install-visit (state snapshot readonly initial)
  "Install SNAPSHOT in STATE's buffer, respecting READONLY and INITIAL."
  (let ((inhibit-read-only t) (create-lockfiles nil) (point-before (point)))
    (save-restriction
      (widen)
      (erase-buffer)
      (insert (plist-get snapshot :text))
      (goto-char (min (point-max) point-before)))
    (when initial
      (setf (tramp-async-files--state-file state) (plist-get snapshot :name)))
    (setq buffer-file-name (tramp-async-files--state-file state)
          default-directory (file-name-directory (plist-get snapshot :path))
          buffer-file-coding-system (plist-get snapshot :coding))
    (tramp-async-files--metadata state snapshot)
    (setq buffer-read-only (or readonly (not (plist-get snapshot :writable))))
    (set-buffer-modified-p nil)
    (when initial
      (tramp-async-files--with-environment
        (tramp-async-files--without-remote-io
          (let ((vc-handled-backends nil) (non-essential t))
            (normal-mode t))))
      (setq buffer-read-only (or buffer-read-only readonly (not (plist-get snapshot :writable))))
      (buffer-enable-undo)
      (tramp-async-files--setup-h)
      (when (and (memq require-final-newline '(visit visit-save))
                 (not buffer-read-only) (> (point-max) (point-min))
                 (not (eq (char-before (point-max)) ?\n)))
        (save-excursion (goto-char (point-max)) (insert "\n")))
      (tramp-async-files--run-hook 'find-file-hook))
    (tramp-async-files--auto-save)
    (force-mode-line-update)
    (message "%s %s" (if (plist-get snapshot :attributes) "Opened" "New file:") buffer-file-name)))

(defun tramp-async-files--visit (file display readonly &optional must-exist)
  "Visit FILE using DISPLAY and READONLY, optionally requiring MUST-EXIST."
  (setq file (tramp-async-files--expand file))
  (if-let* ((existing (tramp-async-files--visiting file)))
      (progn
        (when readonly (with-current-buffer existing (setq buffer-read-only t)))
        (funcall display existing) existing)
    (let* ((buffer (generate-new-buffer (file-name-nondirectory (directory-file-name file))))
           (state (tramp-async-files--state-create :file file :phase 'opening)) job)
      (with-current-buffer buffer
        (setq tramp-async-files--state state buffer-read-only t
              default-directory (file-name-directory file))
        (tramp-async-file-mode 1)
        (condition-case err
            (setq job
                  (tramp-async-call
                   (file-name-directory file) 'tramp-async-files-worker-visit
                   (list file tramp-async-files-size-limit coding-system-for-read
                         (tramp-async-files--locals-enabled-p file) dired-listing-switches
                         (tramp-async-files--envrc-executable) must-exist)
                   (lambda (result)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (when (eq job (tramp-async-files--state-job state))
                           (setf (tramp-async-files--state-job state) nil)
                           (if (not (eq (plist-get result :status) 'ok))
                               (tramp-async-files--failure state result)
                             (let* ((snapshot (plist-get result :value))
                                    (other (tramp-async-files--visiting
                                            (plist-get snapshot (if (plist-get snapshot :directory) :name :path)))))
                               (if (and other (not (eq other buffer)))
                                   (progn
                                     (dolist (window (get-buffer-window-list buffer nil t))
                                       (set-window-buffer window other))
                                     (kill-buffer buffer))
                                 (condition-case err
                                     (if (plist-get snapshot :directory)
                                         (tramp-async-dired--install snapshot t)
                                       (tramp-async-files--install-visit state snapshot readonly t))
                                   ((error quit) (tramp-async-files--failure state (list :error err)))))))))))
                   :library nil :mutating (and (tramp-async-files--envrc-executable) t)))
          (error (tramp-async-files--failure state (list :error err))))
        (setf (tramp-async-files--state-job state) job))
      (funcall display buffer)
      buffer)))

(defun tramp-async-files-find-file (file)
  "Visit FILE asynchronously in the current window."
  (interactive (tramp-async-files--read-args))
  (tramp-async-files--visit file #'pop-to-buffer-same-window nil))

(defun tramp-async-files--revert (_ignore-auto noconfirm)
  "Asynchronously revert this managed buffer, respecting NOCONFIRM."
  (unless (and tramp-async-files--state buffer-file-name)
    (user-error "No successfully visited file to revert"))
  (when (tramp-async-files--state-job tramp-async-files--state)
    (user-error "File operation pending; wait or cancel it first"))
  (unless (or noconfirm (not (buffer-modified-p))
              (yes-or-no-p "Discard these buffer edits and reload the remote file? "))
    (user-error "Revert cancelled"))
  (let ((buffer (current-buffer)) (state tramp-async-files--state)
        (tick (buffer-chars-modified-tick)) (readonly buffer-read-only) job)
    (setq job
          (tramp-async-call
           (file-name-directory buffer-file-name) 'tramp-async-files-worker-visit
           (list buffer-file-name tramp-async-files-size-limit nil
                 (tramp-async-files--locals-enabled-p))
           (lambda (result)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (eq job (tramp-async-files--state-job state))
                   (setf (tramp-async-files--state-job state) nil)
                   (cond
                    ((/= tick (buffer-chars-modified-tick))
                     (tramp-async-files--failure state '(:error (error "Buffer edited during revert; contents preserved"))))
                    ((eq (plist-get result :status) 'ok)
                     (tramp-async-files--install-visit state (plist-get result :value) readonly nil))
                    (t (tramp-async-files--failure state result)))))))
           :library nil :mutating nil))
    (setf (tramp-async-files--state-job state) job (tramp-async-files--state-phase state) 'opening)
    (force-mode-line-update)))

(defun tramp-async-files-revert-buffer (&optional ignore-auto noconfirm preserve-modes)
  "Revert managed buffers asynchronously, using IGNORE-AUTO and NOCONFIRM.
Other buffers retain native behavior, including PRESERVE-MODES."
  (interactive (list current-prefix-arg))
  (cond
   ((tramp-async-dired--remote-p) (tramp-async-dired--adopt) (tramp-async-dired--request))
   ((and tramp-async-file-mode buffer-file-name) (tramp-async-files--revert ignore-auto noconfirm))
   (t (revert-buffer ignore-auto noconfirm preserve-modes))))

(defun tramp-async-files--revert-a (original &optional ignore-auto noconfirm preserve-modes)
  "Adapt interactive reverts; ORIGINAL retains its synchronous Lisp contract."
  (interactive (list current-prefix-arg))
  (if (and (called-interactively-p 'any)
           (or tramp-async-file-mode (tramp-async-dired--remote-p)))
      (tramp-async-files-revert-buffer ignore-auto noconfirm preserve-modes)
    (tramp-async-files--pending-a)
    (funcall original ignore-auto noconfirm preserve-modes)))

(defun tramp-async-files--prepare-save ()
  "Run ordinary text preparation and before-save hooks without remote I/O."
  (when (or write-contents-functions write-file-functions)
    (user-error "Custom write handlers require synchronous saving; disable tramp-async-files-mode first"))
  (save-restriction
    (widen)
    (when (and (> (point-max) (point-min)) (not buffer-read-only)
               (not find-file-literally) (not (eq (char-before (point-max)) ?\n))
               (or (memq require-final-newline '(t visit-save))
                   (and require-final-newline (y-or-n-p "Add a final newline? "))))
      (save-excursion (goto-char (point-max)) (insert "\n")))
    ;; Unlike normal basic-save-buffer, a failed hook aborts this save.  A
    ;; formatter attempting synchronous TRAMP must not silently be skipped.
    (tramp-async-files--without-remote-io (run-hooks 'before-save-hook))))

(defun tramp-async-files-save-buffer (&optional arg)
  "Save a snapshot asynchronously, using ARG's ordinary backup semantics."
  (interactive "p")
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (unless buffer-file-name (user-error "Buffer has no visited file"))
    (unless tramp-async-files--state
      (setq tramp-async-files--state
            (tramp-async-files--state-create :file buffer-file-name
                                            :version (list :mtime (visited-file-modtime)))))
    (tramp-async-file-mode 1)
    (let ((state tramp-async-files--state))
      (unless tramp-async-files--save-followup
        (setq tramp-async-files--format-token nil))
      (cond
       ((eq (tramp-async-files--state-phase state) 'preparing)
        (user-error "Save preparation is already running"))
       ((tramp-async-files--state-job state)
        (if (eq (tramp-async-files--state-phase state) 'saving)
            (progn
              (setf (tramp-async-files--state-again state) (list arg))
              (message "Saving; the latest edits will be saved afterwards"))
          (user-error "File is still loading")))
       (t
        (when-let* ((conflict (tramp-async-files--state-conflict state)))
          (unless (yes-or-no-p (format "Remote file changed. Overwrite %s? " buffer-file-name))
            (user-error "Save not confirmed"))
          (setf (tramp-async-files--state-version state) (plist-get conflict :version)
                (tramp-async-files--state-conflict state) nil))
        (setf (tramp-async-files--state-phase state) 'preparing)
        (condition-case err
            (progn (when (buffer-modified-p) (tramp-async-files--prepare-save))
                   (tramp-async-files--submit-save state arg))
          ((error quit)
           (setf (tramp-async-files--state-phase state) 'error)
           (signal (car err) (cdr err)))))))))

(defun tramp-async-files--submit-save (state arg)
  "Submit the prepared buffer snapshot for STATE using backup ARG."
  (save-restriction
    (widen)
    (let* ((buffer (current-buffer)) (file buffer-file-name)
           (completion tramp-async-files--save-completion)
           (followup tramp-async-files--save-followup)
           (tick (buffer-chars-modified-tick))
           (coding (select-safe-coding-system (point-min) (point-max)
                                              (or coding-system-for-write save-buffer-coding-system
                                                  buffer-file-coding-system)))
           (original-coding buffer-file-coding-system)
           (original-save-coding save-buffer-coding-system)
           (text (buffer-substring-no-properties (point-min) (point-max)))
           (create-parents
            (when (and (tramp-async-files--state-snapshot state)
                       (not (plist-get (tramp-async-files--state-snapshot state) :parent-exists)))
              (unless (yes-or-no-p (format "Create parent directories for %s? " file))
                (user-error "Save cancelled"))
              t))
           (settings (mapcar (lambda (symbol) (cons symbol (symbol-value symbol)))
                             tramp-async-files--save-settings)) job)
      (when (> (string-bytes (encode-coding-string text coding)) tramp-async-files-size-limit)
        (user-error "File exceeds the asynchronous visit/save size limit"))
      (setq job
            (tramp-async-call
             (file-name-directory file) 'tramp-async-files-worker-save
             (list file text coding (tramp-async-files--state-version state)
                   tramp-async-files-size-limit settings arg buffer-backed-up create-parents (buffer-modified-p))
             (lambda (result)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (eq job (tramp-async-files--state-job state))
                     (let ((again (tramp-async-files--state-again state)))
                       (setf (tramp-async-files--state-job state) nil
                             (tramp-async-files--state-again state) nil)
                       (if (not (eq (plist-get result :status) 'ok))
                           (tramp-async-files--failure state result)
                         (if (plist-get (plist-get result :value) :unchanged)
                             (setf (tramp-async-files--state-phase state) nil)
                           (tramp-async-files--metadata state (plist-get result :value))
                           (setq buffer-backed-up (plist-get (plist-get result :value) :backed-up))
                           (if (and (equal file buffer-file-name) (= tick (buffer-chars-modified-tick))
                                    (eq original-coding buffer-file-coding-system)
                                    (eq original-save-coding save-buffer-coding-system))
                               (progn
                                 (setq buffer-file-coding-system coding)
                                 (set-buffer-modified-p nil)
                                 (when (and buffer-auto-save-file-name
                                            (not (file-remote-p buffer-auto-save-file-name)))
                                   (delete-auto-save-file-if-necessary))
                                 (let ((apheleia-format-after-save-in-progress followup))
                                   (condition-case err
                                       (tramp-async-files--run-hook 'after-save-hook)
                                     ((error quit)
                                      (setq result (list :status 'error :error err))
                                      (tramp-async-files--failure state result)))))
                             ;; Undo can restore a clean flag while a snapshot
                             ;; is in flight; disk now contains that snapshot.
                             (set-buffer-modified-p t)))
                         (force-mode-line-update)
                         (message "%s %s%s"
                                  (if (plist-get (plist-get result :value) :unchanged) "Unchanged:" "Saved")
                                  file (cond ((eq (plist-get result :status) 'error) " (after-save hook failed)")
                                             ((buffer-modified-p) " (newer buffer edits remain)") (t "")))
                         (when (and (eq (plist-get result :status) 'ok) again (buffer-modified-p))
                           (tramp-async-files-save-buffer (car again))))
                     (when completion (funcall completion result)))))))
             :library nil :write-buffer buffer))
      (setf (tramp-async-files--state-job state) job
            (tramp-async-files--state-tick state) tick
            (tramp-async-files--state-phase state) 'saving)
      (force-mode-line-update)
      (message "Saving %s asynchronously…" file))))

(defun tramp-async-files--read-args (&optional prompt directory default)
  "Read with worker completion, using PROMPT, DIRECTORY and DEFAULT."
  (let ((cache (make-hash-table :test #'equal)) jobs (active t)
        (base (or directory default-directory)))
    (unwind-protect
        (list
         (completing-read
          (or prompt "Find file: ")
          (lambda (input predicate action)
            (if (eq action 'metadata) '(metadata (category . tramp-async-file))
              (let* ((name (tramp-async-files--expand input base))
                     (directory (file-name-directory name))
                     (cached (gethash directory cache 'missing)))
                (when (eq cached 'missing)
                  (if (not (file-remote-p directory))
                      (setq cached (file-name-all-completions "" directory))
                    (setq cached nil)
                    (puthash directory nil cache)
                    (push
                     (tramp-async-call
                      directory 'tramp-async-files-worker-completions (list directory)
                      (lambda (result)
                        (when active
                          (if (not (eq (plist-get result :status) 'ok))
                              (progn
                                (remhash directory cache)
                                (minibuffer-message " Completion failed: %s"
                                                    (error-message-string (plist-get result :error))))
                            (puthash directory (plist-get result :value) cache)
                            (when-let* ((window (active-minibuffer-window)))
                              (with-selected-window window
                                (when (equal input (minibuffer-contents-no-properties))
                                  (minibuffer-complete)))))))
                      :library nil :mutating nil)
                     jobs)))
                (complete-with-action
                 action (mapcar (lambda (entry) (concat (or (file-name-directory input) "") entry)) cached)
                 input predicate))))
          nil nil base 'file-name-history default))
      (setq active nil)
      (dolist (job jobs) (tramp-async-cancel job)))))

(defmacro tramp-async-files--visit-advice (name display readonly)
  "Define NAME to adapt an interactive visit with DISPLAY and READONLY."
  `(defun ,name (original filename &optional wildcards)
     "Adapt an interactive remote visit; ORIGINAL handles other calls."
     (interactive (append (tramp-async-files--read-args) '(t)))
     (let ((file (and (called-interactively-p 'any) (tramp-async-files--expand filename))))
       (if (and file (file-remote-p file))
           (progn
             (when (and wildcards find-file-wildcards (string-match-p "[[*?]" file))
               (user-error "Async file visiting requires a single filename"))
             (tramp-async-files--visit file ,display ,readonly))
         (funcall original filename wildcards)))))
(tramp-async-files--visit-advice tramp-async-files--find-a #'pop-to-buffer-same-window nil)
(tramp-async-files--visit-advice tramp-async-files--other-window-a #'switch-to-buffer-other-window nil)
(tramp-async-files--visit-advice tramp-async-files--other-frame-a #'switch-to-buffer-other-frame nil)
(tramp-async-files--visit-advice tramp-async-files--readonly-a #'pop-to-buffer-same-window t)
(tramp-async-files--visit-advice tramp-async-files--readonly-window-a #'switch-to-buffer-other-window t)
(tramp-async-files--visit-advice tramp-async-files--readonly-frame-a #'switch-to-buffer-other-frame t)

(defun tramp-async-files--save-a (original &optional arg)
  "Adapt interactive remote saves; ORIGINAL retains synchronous Lisp calls."
  (interactive "p")
  (if (and (called-interactively-p 'any) buffer-file-name (file-remote-p buffer-file-name))
      (tramp-async-files-save-buffer arg)
    (funcall original arg)))
(defun tramp-async-files--pending-a (&rest _)
  "Reject synchronous saving or filename changes during asynchronous work."
  (when (and tramp-async-files--state
             (or (tramp-async-files--state-job tramp-async-files--state)
                 (eq (tramp-async-files--state-phase tramp-async-files--state) 'preparing)))
    (user-error "Asynchronous file operation pending; wait or cancel it first")))
(defun tramp-async-files--exit-query-h ()
  "Keep Emacs alive until pending asynchronous saves are acknowledged."
  (not (cl-some (lambda (buffer)
                  (with-current-buffer buffer (not (tramp-async-files--kill-query-h))))
                (buffer-list))))

(defconst tramp-async-files--advices
  '((find-file . tramp-async-files--find-a)
    (find-file-other-window . tramp-async-files--other-window-a)
    (find-file-other-frame . tramp-async-files--other-frame-a)
    (find-file-read-only . tramp-async-files--readonly-a)
    (find-file-read-only-other-window . tramp-async-files--readonly-window-a)
    (find-file-read-only-other-frame . tramp-async-files--readonly-frame-a)
    (save-buffer . tramp-async-files--save-a)
    (revert-buffer . tramp-async-files--revert-a)
    (normal-mode . tramp-async-files--normal-mode-a)
    (hack-dir-local--get-variables . tramp-async-files--locals-a)))

(defun tramp-async-files--apheleia-a (original)
  "Keep ORIGINAL for ordinary buffers; await formatting and worker saves here."
  (if (not (and tramp-async-files-mode tramp-async-file-mode buffer-file-name))
      (funcall original)
    (unless apheleia-format-after-save-in-progress
      (when (and apheleia-mode (not (buffer-narrowed-p)))
        (when-let* ((formatters (apheleia--get-formatters)))
          (let ((buffer (current-buffer)) (state tramp-async-files--state)
                (file buffer-file-name) (tick (buffer-chars-modified-tick))
                (token (make-symbol "format")))
            (setq tramp-async-files--format-token token)
            (condition-case err
                (apheleia-format-buffer
                 formatters nil
                 :callback
                 (lambda (&rest result)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (when (and (eq token tramp-async-files--format-token)
                                  (eq state tramp-async-files--state) (equal file buffer-file-name))
                         (if-let* ((err (plist-get result :error)))
                             (progn
                               (setq tramp-async-files--format-token nil)
                               ;; Apheleia discards output if the user edited meanwhile.
                               (when (= tick (buffer-chars-modified-tick))
                                 (tramp-async-files--failure state (list :error err))))
                           (let ((tramp-async-files--save-followup t)
                                 (tramp-async-files--save-completion
                                  (lambda (saved)
                                    (when (eq token tramp-async-files--format-token)
                                      (setq tramp-async-files--format-token nil)
                                      (when (eq (plist-get saved :status) 'ok)
                                        (tramp-async-files--run-hook 'apheleia-post-format-hook))))))
                             (condition-case err (tramp-async-files-save-buffer)
                               ((error quit)
                                (setq tramp-async-files--format-token nil)
                                (tramp-async-files--failure state (list :error err)))))))))))
              ((error quit) (setq tramp-async-files--format-token nil)
                     (signal (car err) (cdr err))))))))))

(defvar denote-dired-directories)
(defun tramp-async-files--denote-a (original)
  "Apply ORIGINAL only to configured Denote roots on this buffer's host."
  (if (not (and tramp-async-file-mode (file-remote-p default-directory nil 'never)))
      (funcall original)
    (let* ((host (file-remote-p default-directory nil 'never))
           (denote-dired-directories
            (cl-remove-if-not
             (lambda (dir) (and (stringp dir) (equal host (file-remote-p dir nil 'never))))
             denote-dired-directories)))
      (when denote-dired-directories (funcall original)))))

(defun tramp-async-files--extension-integration (enable)
  "Install explicit extension adapters according to ENABLE."
  (dolist (entry '((apheleia-format-after-save . tramp-async-files--apheleia-a)
                   (denote-dired-mode-in-directories . tramp-async-files--denote-a)))
    (if enable (advice-add (car entry) :around (cdr entry))
      (advice-remove (car entry) (cdr entry)))))

;;;###autoload
(define-minor-mode tramp-async-files-mode
  "Use workers for interactive remote file opening, completion and saving."
  :global t :group 'tramp-async-files
  (tramp-async-files--extension-integration tramp-async-files-mode)
  (when (fboundp 'envrc--get-current-env-or-run-direnv)
    (if tramp-async-files-mode
        (advice-add 'envrc--get-current-env-or-run-direnv :around #'tramp-async-files--envrc-a)
      (advice-remove 'envrc--get-current-env-or-run-direnv #'tramp-async-files--envrc-a)))
  (tramp-async-dired-integration tramp-async-files-mode)
  (tramp-async-batch-integration tramp-async-files-mode)
  (if tramp-async-files-mode
      (progn
        (dolist (entry tramp-async-files--advices) (advice-add (car entry) :around (cdr entry)))
        (advice-add 'tramp-file-name-handler :around #'tramp-async-files--io-a)
        (advice-remove 'basic-save-buffer #'tramp-async-files--pending-a)
        (advice-add 'basic-save-buffer :around #'tramp-async-files--basic-save-a)
        (advice-add 'set-visited-file-name :before #'tramp-async-files--pending-a)
        (add-hook 'kill-emacs-query-functions #'tramp-async-files--exit-query-h))
    (dolist (entry tramp-async-files--advices) (advice-remove (car entry) (cdr entry)))
    (advice-remove 'basic-save-buffer #'tramp-async-files--pending-a)
    (advice-remove 'basic-save-buffer #'tramp-async-files--basic-save-a)
    (advice-remove 'tramp-file-name-handler #'tramp-async-files--io-a)
    (advice-remove 'set-visited-file-name #'tramp-async-files--pending-a)
    (remove-hook 'kill-emacs-query-functions #'tramp-async-files--exit-query-h)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer (when tramp-async-file-mode (tramp-async-file-mode -1))))))

(defun tramp-async-files-unload-function ()
  "Remove command integration before unloading."
  (tramp-async-files-mode -1)
  nil)

;;; Interactive Dired mutations

(require 'dired-aux)


(cl-defstruct (tramp-async-dired-ops--operation (:constructor tramp-async-dired-ops--create))
              buffer kind files marks marker job locks results active)
(defvar tramp-async-dired-ops--operations nil)
(defvar-local tramp-async-dired-ops--lock nil)
(put 'tramp-async-dired-ops--lock 'permanent-local t)
(put 'tramp-async-dired-ops--locked-a 'permanent-local-hook t)
(put 'tramp-async-dired-ops--kill-query-h 'permanent-local-hook t)
(defvar-local tramp-async-dired-ops-last-result nil
  "Last batch result, including acknowledged items and uncertain outcomes.")
(defvar-local tramp-async-dired--pending-marks nil)
(defvar dired-subtree-overlays)
(defvar tramp-async-files-mode)

(defun tramp-async-dired-ops--relative (name true descriptor &optional writing)
  "Map NAME or TRUE into DESCRIPTOR, returning a relative suffix or nil.
WRITING also matches a final symlink's referent; moving/deleting does not."
  (when descriptor
    (let ((root (plist-get descriptor :name)) (entry (plist-get descriptor :entry))
          (tree (plist-get descriptor :tree)))
      (cond
       ((equal name root) "")
       ((and tree name (string-prefix-p (file-name-as-directory root) name))
        (substring name (length root)))
       ((and (not (plist-get descriptor :link)) true (equal true entry)) "")
       ((and tree true (string-prefix-p (file-name-as-directory entry) true))
        (substring true (length entry)))
       ((and writing true (equal true (plist-get descriptor :referent))) "")
       ((and (or writing (not (plist-get descriptor :link)))
             (assoc name (plist-get descriptor :aliases)))
        (cdr (assoc name (plist-get descriptor :aliases))))))))

(defun tramp-async-dired-ops--locked-a (&rest _)
  "Prevent edits, saves and visited-name changes during a Dired mutation."
  (when tramp-async-dired-ops--lock
    (user-error "Dired operation pending for this buffer; wait or cancel in its Dired buffer")))
(defun tramp-async-dired-ops--kill-query-h ()
  "Keep a protected buffer alive until its file operation is acknowledged."
  (if tramp-async-dired-ops--lock
      (progn (message "Dired operation pending; wait or cancel it first") nil) t))

(defun tramp-async-dired-ops--unlock (operation)
  "Release all buffer protections owned by OPERATION."
  (dolist (buffer (tramp-async-dired-ops--operation-locks operation))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq tramp-async-dired-ops--lock operation)
          (setq tramp-async-dired-ops--lock nil)
          (remove-hook 'before-change-functions #'tramp-async-dired-ops--locked-a t)
          (remove-hook 'kill-buffer-query-functions #'tramp-async-dired-ops--kill-query-h t)))))
  (setf (tramp-async-dired-ops--operation-locks operation) nil))

(defun tramp-async-dired-ops--guard (operation item)
  "Protect visiting buffers affected by OPERATION's ITEM before mutation."
  (setf (tramp-async-dired-ops--operation-active operation) item)
  (when (eq (plist-get item :operation) 'rename)
    (let (source-buffer target-buffer)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when buffer-file-name
            (if (tramp-async-dired-ops--relative buffer-file-name buffer-file-truename (plist-get item :source))
                (setq source-buffer buffer)
              (when (tramp-async-dired-ops--relative buffer-file-name buffer-file-truename (plist-get item :target) t)
                (setq target-buffer buffer))))))
      (when (and source-buffer target-buffer
                 (not (y-or-n-p (format "A buffer is visiting %s; proceed? "
                                        (buffer-local-value 'buffer-file-name target-buffer)))))
        (user-error "Aborted"))))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (let* ((state tramp-async-files--state)
             (name (or buffer-file-name (and state (tramp-async-files--state-file state))))
             (source (and (not (eq (plist-get item :operation) 'copy))
                          (tramp-async-dired-ops--relative name buffer-file-truename (plist-get item :source))))
             (target (tramp-async-dired-ops--relative name buffer-file-truename (plist-get item :target) t)))
        (when (and name (or source target)
                   (not (eq buffer (tramp-async-dired-ops--operation-buffer operation))))
          (when (or (and tramp-async-dired-ops--lock (not (eq tramp-async-dired-ops--lock operation)))
                    (and state (tramp-async-files--state-job state)))
            (error "Another file operation is pending: %s" name))
          (when (and target (not source) buffer-file-name (buffer-modified-p))
            (error "Destination has unsaved Emacs edits: %s" name))
          (setq tramp-async-dired-ops--lock operation)
          (cl-pushnew buffer (tramp-async-dired-ops--operation-locks operation))
          (add-hook 'before-change-functions #'tramp-async-dired-ops--locked-a nil t)
          (add-hook 'kill-buffer-query-functions #'tramp-async-dired-ops--kill-query-h nil t))))))

(defun tramp-async-dired-ops--move-recovery ()
  "Give this renamed buffer a separate local recovery file.
Never overwrite an existing recovery file or inspect a remote auto-save."
  (when buffer-auto-save-file-name
    (when (file-remote-p tramp-async-files-auto-save-directory)
      (error "Async recovery directory must be local"))
    (let* ((old buffer-auto-save-file-name)
           (new (expand-file-name (concat "#" (secure-hash 'sha256 buffer-file-name) "#")
                                  tramp-async-files-auto-save-directory)))
      (make-directory tramp-async-files-auto-save-directory t)
      (setq-local remote-file-name-inhibit-auto-save nil)
      (unless (equal old new)
        (when (file-exists-p new) (setq new (make-temp-name (concat new ".renamed-"))))
        (setq buffer-auto-save-file-name new)
        (setf (alist-get 'buffer-auto-save-file-name tramp-async-files--original-locals)
              (list t new))
        (when (and (not (file-remote-p old)) (file-exists-p old))
          (condition-case err (rename-file old new nil)
            (file-error
             (display-warning 'tramp-async-files
                              (format "Recovery retained at %s: %s" old (error-message-string err))))))
        ;; This also refreshes the session recovery index for the new name.
        (let ((default-directory temporary-file-directory))
          (do-auto-save t t))))))

(defun tramp-async-dired-ops--retarget (item)
  "Retarget source visiting buffers after a successful rename ITEM.
Use only in-memory metadata.  Retain text, dirty state and the old visited
modtime, so subsequent saves still detect external changes."
  (let ((source (plist-get item :source)) (target (plist-get item :target)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when-let* ((relative (and buffer-file-name
                                   (tramp-async-dired-ops--relative buffer-file-name buffer-file-truename source))))
          (let ((name (concat (plist-get target :name) relative))
                (true (concat (plist-get target :entry) relative))
                (state (or tramp-async-files--state
                           (setq tramp-async-files--state
                                 (tramp-async-files--state-create)))))
            (setq buffer-file-name name buffer-file-truename true
                  default-directory (file-name-directory name) buffer-backed-up nil
                  buffer-file-number nil)
            (unless (equal (file-name-nondirectory name) (buffer-name))
              (rename-buffer (file-name-nondirectory name) t))
            (tramp-async-file-mode 1)
            (let ((old-recovery buffer-auto-save-file-name))
              (condition-case err (tramp-async-dired-ops--move-recovery)
                (error
                 (when (equal old-recovery buffer-auto-save-file-name)
                   (setq buffer-auto-save-file-name nil))
                 (display-warning 'tramp-async-files
                                  (format "Could not update recovery file %s: %s"
                                          old-recovery (error-message-string err))))))
            (when state
              (setf (tramp-async-files--state-file state) name
                    (tramp-async-files--state-version state) (list :mtime (visited-file-modtime))
                    (tramp-async-files--state-conflict state) nil)
              (let ((snapshot (tramp-async-files--state-snapshot state)))
                (setf (tramp-async-files--state-snapshot state)
                      (plist-put (plist-put snapshot :name name) :path true))))))
        ;; Rebase every inserted/subtree directory, including those in the
        ;; parent listing; otherwise its refresh would read removed paths.
        (when (derived-mode-p 'dired-mode)
          (let ((rebase
                 (lambda (directory)
                   (if-let* ((suffix (tramp-async-dired-ops--relative
                                      (directory-file-name directory) nil source)))
                       (file-name-as-directory (concat (plist-get target :name) suffix))
                     directory))))
            (setq dired-subdir-alist
                  (mapcar (lambda (entry) (cons (funcall rebase (car entry)) (cdr entry)))
                          dired-subdir-alist))
            (when (boundp 'dired-subtree-overlays)
              (dolist (overlay dired-subtree-overlays)
                (when-let* ((dir (overlay-get overlay 'dired-subtree-name)))
                  (overlay-put overlay 'dired-subtree-name
                               (directory-file-name (funcall rebase dir))))))
            (let ((name (funcall rebase default-directory)))
              (unless (equal name default-directory)
                (setq default-directory name dired-directory name)
                (when tramp-async-files--state
                  (setf (tramp-async-files--state-file tramp-async-files--state) name))))))))))

(defun tramp-async-dired-ops--prune-deleted (item)
  "Remove expanded directory sections affected by deleted ITEM from the UI."
  (let ((source (plist-get item :source)))
    (dolist (entry (copy-sequence dired-subdir-alist))
      (when (and (not (equal (car entry) default-directory))
                 (tramp-async-dired-ops--relative (directory-file-name (car entry)) nil source)
                 (dired-goto-subdir (car entry)))
        (dired-kill-subdir)))
    (when (boundp 'dired-subtree-overlays)
      (setq dired-subtree-overlays
            (cl-delete-if
             (lambda (overlay)
               (when-let* ((name (overlay-get overlay 'dired-subtree-name)))
                 (when (tramp-async-dired-ops--relative (directory-file-name name) nil source)
                   (when (overlay-buffer overlay)
                     (delete-region (overlay-start overlay) (overlay-end overlay)))
                   (delete-overlay overlay)
                   t)))
             dired-subtree-overlays)))))

(defun tramp-async-dired-ops--record (operation item)
  "Record acknowledged ITEM for OPERATION and reconcile buffer names."
  (push item (tramp-async-dired-ops--operation-results operation))
  (unwind-protect
      (when (eq (plist-get item :status) 'ok)
        (when (eq (plist-get item :operation) 'rename) (tramp-async-dired-ops--retarget item))
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (save-excursion
              (when (and (tramp-async-dired--managed-p)
                         (cl-some (lambda (file)
                                    (and file (string-prefix-p default-directory file)))
                                  (list (plist-get item :from) (plist-get item :to))))
                (tramp-async-files--without-remote-io
                  (let ((inhibit-read-only t) (inhibit-modification-hooks t) (from (plist-get item :from)) (to (plist-get item :to)))
                    (when (eq (plist-get item :operation) 'delete)
                      (tramp-async-dired-ops--prune-deleted item))
                    (when (and (memq (plist-get item :operation) '(rename delete))
                               (dired-goto-file from))
                      (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position)))))
                    (when (and to (string-prefix-p default-directory to))
                      (let ((mark (tramp-async-dired-ops--operation-marker operation)))
                        (when (eq mark t) (setq mark (cdr (assoc from (tramp-async-dired-ops--operation-marks operation)))))
                        (when (integerp mark) (push (cons to mark) tramp-async-dired--pending-marks)))))))))))
    (tramp-async-dired-ops--unlock operation)
    (setf (tramp-async-dired-ops--operation-active operation) nil)))

(defun tramp-async-dired-ops--event (operation event)
  "Handle a typed worker EVENT for OPERATION without remote I/O."
  (unless (memq operation tramp-async-dired-ops--operations) (error "Dired operation was cancelled"))
  (pcase (plist-get event :event)
    ('query
     (let ((kind (plist-get event :kind)))
       (unless (memq kind '(yes-or-no-p y-or-n-p read-char-choice read-answer))
         (error "Unsupported Dired question: %s" kind))
       (let ((enable-recursive-minibuffers t))
         (apply kind (plist-get event :args)))))
    ('guard (tramp-async-dired-ops--guard operation (plist-get event :item)))
    ('done (tramp-async-dired-ops--record operation (plist-get event :item)))
    (_ (error "Unknown Dired worker event"))))

(defun tramp-async-dired-ops--cleanup-deleted (items)
  "Offer native buffer cleanup for successfully deleted ITEMS."
  (when (and (bound-and-true-p dired-clean-up-buffers-too) (featurep 'dired-x))
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((name (or buffer-file-name (and (derived-mode-p 'dired-mode) (directory-file-name default-directory)))))
            (when (and name
                       (cl-some (lambda (item)
                                  (and (eq (plist-get item :operation) 'delete)
                                       (eq (plist-get item :status) 'ok)
                                       (tramp-async-dired-ops--relative name buffer-file-truename (plist-get item :source)))) items)
                       (or (not dired-clean-confirm-killing-deleted-buffers)
                           (y-or-n-p (format "Kill buffer of %s, too? " name))))
              (tramp-async-files--without-remote-io (kill-buffer buffer)))))))))

(defun tramp-async-dired-ops--finish (operation result)
  "Finish OPERATION with RESULT, preserving acknowledged partial progress."
  (tramp-async-dired-ops--unlock operation)
  (setq tramp-async-dired-ops--operations (delq operation tramp-async-dired-ops--operations))
  (let* ((items (reverse (tramp-async-dired-ops--operation-results operation)))
         (error-data (or (plist-get result :error) (plist-get (plist-get result :value) :error)))
         (uncertain (or (plist-get result :uncertain)
                        (and error-data (tramp-async-dired-ops--operation-active operation))
                        (cl-some (lambda (item) (plist-get item :uncertain)) items)))
         (report (list :items items :error error-data :uncertain (and uncertain t)
                       :remaining (cl-set-difference (tramp-async-dired-ops--operation-files operation)
                                                     (mapcar (lambda (item) (plist-get item :from)) items) :test #'equal)))
         (origin (tramp-async-dired-ops--operation-buffer operation)))
    (when (buffer-live-p origin)
      (with-current-buffer origin
        (setq tramp-async-dired-ops-last-result report)
        (when (and tramp-async-files--state
                   (eq (tramp-async-files--state-job tramp-async-files--state)
                       (tramp-async-dired-ops--operation-job operation)))
          (setf (tramp-async-files--state-job tramp-async-files--state) nil
                (tramp-async-files--state-phase tramp-async-files--state) nil))))
    (dolist (item items)
      (when (plist-get item :error)
        (dired-log "%s: %s: %s\n" (plist-get item :operation) (plist-get item :from)
                   (error-message-string (plist-get item :error)))))
    (when error-data (dired-log "Async Dired: %s\n" (error-message-string error-data)))
    (when uncertain (dired-log "Interrupted/failed mutation may be partial; inspect before retrying.\n"))
    ;; Schedule refresh before optional cleanup prompts.  All listings still
    ;; come from workers.  A currently busy listing retains its own request.
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (buffer-live-p origin) (buffer-local-value 'tramp-async-file-mode origin)
                   (derived-mode-p 'dired-mode)
                   (or (eq buffer origin)
                       (cl-some (lambda (item)
                                  (cl-some (lambda (file)
                                             (and file (or (string-prefix-p default-directory file)
                                                           (string-prefix-p (file-name-as-directory file) default-directory))))
                                           (list (plist-get item :from) (plist-get item :to)))) items))
                   (not (and tramp-async-files--state (tramp-async-files--state-job tramp-async-files--state))))
          (tramp-async-dired--adopt)
          (tramp-async-dired--request))))
    (condition-case err (tramp-async-dired-ops--cleanup-deleted items)
      ((error quit) (dired-log "Buffer cleanup: %s\n" (error-message-string err))))
    (message "Dired %s: %d completed, %d skipped/failed%s%s"
             (tramp-async-dired-ops--operation-kind operation)
             (cl-count 'ok items :key (lambda (item) (plist-get item :status)))
             (cl-count-if (lambda (item) (not (eq (plist-get item :status) 'ok))) items)
             (if error-data " (stopped; see *Dired log*)" "")
             (if uncertain " (outcome may be partial; inspect before retrying)" ""))))

(defun tramp-async-dired-operate (kind files &optional target arg)
  "Asynchronously perform KIND on absolute FILES, optionally to TARGET.
KIND is copy, rename or delete.  This explicit API assumes the initial
selection/delete confirmation was obtained by its caller.
Requires `tramp-async-files-mode'.  Native worker
conflict/recursive questions still run; ARG preserves copy dereferencing."
  (unless tramp-async-files-mode (user-error "Enable tramp-async-files-mode first"))
  (tramp-async-dired--adopt)
  (tramp-async-files--pending-a)
  (tramp-async-dired-ops--locked-a)
  (when (and (eq kind 'copy) dired-copy-how-to-fn)
    (user-error "Custom archive copy handlers require synchronous Dired"))
  (unless (memq kind '(copy rename delete)) (user-error "Unknown Dired operation"))
  (dolist (file files)
    (when (member (file-name-nondirectory (directory-file-name file)) '("." ".." ""))
      (user-error "Cannot operate on dot entries or a filesystem root")))
  (let* ((operation (tramp-async-dired-ops--create
                     :buffer (current-buffer) :kind kind :files files
                     :marks (dired-remember-marks (point-min) (point-max))
                     :marker (pcase kind ('copy dired-keep-marker-copy) ('rename dired-keep-marker-rename))))
         (settings (mapcar (lambda (symbol) (cons symbol (symbol-value symbol)))
                           tramp-async-dired-ops-worker-settings)) job)
    (push operation tramp-async-dired-ops--operations)
    (condition-case err
        (setq job (tramp-async-call
                   default-directory 'tramp-async-dired-ops-worker-run (list kind files target settings arg)
                   (lambda (result) (tramp-async-dired-ops--finish operation result))
                   :library nil
                   :event-handler (lambda (event) (tramp-async-dired-ops--event operation event))))
      (error (setq tramp-async-dired-ops--operations (delq operation tramp-async-dired-ops--operations))
             (signal (car err) (cdr err))))
    (setf (tramp-async-dired-ops--operation-job operation) job
          (tramp-async-files--state-job tramp-async-files--state) job
          (tramp-async-files--state-phase tramp-async-files--state) 'mutating)
    (setq tramp-async-dired-ops-last-result nil)
    (force-mode-line-update)
    job))

(defun tramp-async-dired-ops--command (kind arg &optional flagged nomessage)
  "Read native selection/confirmation for KIND and ARG.
FLAGGED selects deletion flags; NOMESSAGE suppresses empty-selection messages."
  (tramp-async-files--pending-a)
  (let* ((dired-marker-char (if flagged dired-del-marker dired-marker-char))
         (files (dired-get-marked-files nil (if flagged 'marked arg) nil nil t))
         (relative (mapcar #'dired-make-relative files)))
    (if (not files) (unless nomessage (message "(No files selected)"))
      (if (eq kind 'delete)
          (when (dired-mark-pop-up
                 " *Deletions*" 'delete relative dired-deletion-confirmer
                 (format "%s %s " (if delete-by-moving-to-trash "Trash" "Delete")
                         (dired-mark-prompt arg relative)))
            (tramp-async-dired-operate kind files nil arg))
        (let* ((dir (dired-dwim-target-directory))
               (defaults (dired-dwim-target-defaults files dir))
               (target (cl-letf (((symbol-function 'read-file-name)
                                  (lambda (prompt &optional directory default &rest _)
                                    (car (tramp-async-files--read-args prompt directory default)))))
                                (dired-mark-read-file-name
                                 (format "%s %%s to: " (if (eq kind 'copy) "Copy" "Rename"))
                                 dir (if (eq kind 'copy) 'copy 'move) arg relative defaults))))
          (tramp-async-dired-operate kind files (tramp-async-files--expand target) arg)))
      (dired-post-do-command))))

(defmacro tramp-async-dired-ops--advice (name kind &optional flagged)
  "Define NAME for an interactive KIND adapter, optionally FLAGGED."
  `(defun ,name (original &optional arg)
     "Adapt interactive remote Dired mutations; preserve ORIGINAL's Lisp contract."
     (interactive ,(unless flagged "P"))
     (if (and (called-interactively-p 'any)
              (or (tramp-async-dired--managed-p) (tramp-async-dired--remote-p)))
         (tramp-async-dired-ops--command ',kind ,(unless flagged 'arg) ,flagged ,(when flagged 'arg))
       (funcall original arg))))
(tramp-async-dired-ops--advice tramp-async-dired-ops--copy-a copy)
(tramp-async-dired-ops--advice tramp-async-dired-ops--rename-a rename)
(tramp-async-dired-ops--advice tramp-async-dired-ops--delete-a delete)
(tramp-async-dired-ops--advice tramp-async-dired-ops--flagged-a delete t)

(defun tramp-async-dired-ops-integration (enable)
  "Install or remove mutation command adapters according to ENABLE."
  (unless enable
    (dolist (operation tramp-async-dired-ops--operations)
      (tramp-async-cancel (tramp-async-dired-ops--operation-job operation))
      (tramp-async-dired-ops--unlock operation)))
  (dolist (entry '((dired-do-copy . tramp-async-dired-ops--copy-a)
                   (dired-do-rename . tramp-async-dired-ops--rename-a)
                   (dired-do-delete . tramp-async-dired-ops--delete-a)
                   (dired-do-flagged-delete . tramp-async-dired-ops--flagged-a)))
    (if enable (advice-add (car entry) :around (cdr entry))
      (advice-remove (car entry) (cdr entry))))
  (if enable
      (add-hook 'tramp-async-before-buffer-write-hook #'tramp-async-dired-ops--locked-a)
    (remove-hook 'tramp-async-before-buffer-write-hook #'tramp-async-dired-ops--locked-a))
  (dolist (function '(basic-save-buffer tramp-async-files-save-buffer set-visited-file-name))
    (if enable (advice-add function :before #'tramp-async-dired-ops--locked-a)
      (advice-remove function #'tramp-async-dired-ops--locked-a))))

;;; Interactive Dired browsing

(require 'dired)

(defvar tramp-async-dired--subtree-listings nil)
(defvar-local tramp-async-dired--previous-buffer nil)
(put 'tramp-async-dired--previous-buffer 'permanent-local t)
(declare-function dired-subtree--dired-line-is-directory-or-link-p "dired-subtree" ())
(declare-function dired-subtree--is-expanded-p "dired-subtree" ())
(defvar dired-subtree-overlays)

(defun tramp-async-dired--remote-p ()
  "Whether this is a remote Dired buffer eligible for adaptation."
  (and (derived-mode-p 'dired-mode) (file-remote-p default-directory nil 'never)))

(defun tramp-async-dired--expand (file &optional directory)
  "Expand FILE in DIRECTORY with the current buffer's observed path aliases."
  (tramp-async-files--expand-aliases
   file directory (and tramp-async-files--state
                       (plist-get (tramp-async-files--state-snapshot tramp-async-files--state) :aliases))))

(defun tramp-async-dired--adopt ()
  "Adopt a previously synchronous Dired buffer without reading the directory."
  (unless tramp-async-files--state
    (when (consp dired-directory)
      (user-error "Async Dired requires a directory rather than an explicit file list"))
    (setq tramp-async-files--state (tramp-async-files--state-create :file dired-directory)))
  (tramp-async-file-mode 1)
  (setq-local revert-buffer-function #'dired-revert))

(defun tramp-async-dired--managed-p ()
  "Whether this is a managed Dired buffer."
  (and tramp-async-file-mode (derived-mode-p 'dired-mode)))

(defun tramp-async-dired--install (snapshot &optional initial)
  "Install the worker SNAPSHOT; INITIAL initializes a new Dired buffer."
  (let* ((state tramp-async-files--state)
         (directory (plist-get snapshot :name))
         (inhibit-read-only t)
         (positions (unless initial (dired-save-positions)))
         (hidden (unless initial (dired-remember-hidden)))
         (tramp-async-dired--subtree-listings (plist-get snapshot :subtrees))
         (marks (unless initial (dired-remember-marks (point-min) (point-max)))))
    (setf (tramp-async-files--state-snapshot state) snapshot
          (tramp-async-files--state-file state) directory
          (tramp-async-files--state-phase state) nil
          (tramp-async-files--state-error state) nil)
    (setq default-directory directory buffer-file-name nil)
    (tramp-async-files--with-environment
     (tramp-async-files--without-remote-io
      (when initial
        ;; Global minor modes run from the major-mode lifecycle hooks too.
        ;; Isolate their synchronous I/O just like explicit Dired hooks, so
        ;; one extension cannot abort installation of the worker listing.
        (let ((dired-mode-hook nil)
              (change-major-mode-after-body-hook nil)
              (after-change-major-mode-hook nil))
          (dired-mode directory (plist-get snapshot :switches)))
        (tramp-async-files--run-hook 'change-major-mode-after-body-hook)
        (tramp-async-files--setup-h))
      ;; An adopted native Dired buffer may retain a ~/ abbreviation here.
      ;; Refresh does not rerun `dired-mode', so reconcile its directory with
      ;; the worker's expanded name before hooks query the snapshot metadata.
      (setq dired-directory directory list-buffers-directory directory)
      (unless initial (hack-dir-local-variables-non-file-buffer))
      (setq dired-actual-switches (plist-get snapshot :switches))
      (widen)
      (erase-buffer)
      (insert (plist-get snapshot :text))
      (setq dired-subdir-alist nil)
      (dired-build-subdir-alist)
      (dired-insert-set-properties (point-min) (point-max))
      (when marks (dired-mark-remembered marks))
      (tramp-async-files--run-hook 'dired-after-readin-hook)
      (when marks (dired-mark-remembered marks))
      (when tramp-async-dired--pending-marks
        (dired-mark-remembered tramp-async-dired--pending-marks)
        (setq tramp-async-dired--pending-marks nil))
      (when initial
        (tramp-async-files--run-hook 'dired-mode-hook)
        (tramp-async-files--run-hook 'after-change-major-mode-hook))
      (if positions (dired-restore-positions positions)
        (goto-char (point-min)) (dired-next-line 2))
      (save-excursion
        (dolist (dir hidden)
          (when (dired-goto-subdir dir) (dired-hide-subdir 1))))))
    ;; Retain data, not the entire directory listing, in the guard cache.
    (setf (tramp-async-files--state-snapshot state) (plist-put snapshot :text nil))
    (setf (alist-get 'revert-buffer-function tramp-async-files--original-locals)
          '(t dired-revert))
    (unless (assq 'buffer-stale-function tramp-async-files--original-locals)
      (push '(buffer-stale-function t dired-buffer-stale-p) tramp-async-files--original-locals))
    (setq-local revert-buffer-function #'dired-revert)
    (setq-local buffer-stale-function #'ignore)
    (set-buffer-modified-p nil)
    (force-mode-line-update)
    (when (and initial (buffer-live-p tramp-async-dired--previous-buffer)
               (not (get-buffer-window tramp-async-dired--previous-buffer t)))
      (kill-buffer tramp-async-dired--previous-buffer))
    (setq tramp-async-dired--previous-buffer nil)
    (message "Read directory %s" directory)))

(defun tramp-async-dired--request (&optional subdirs callback show-omitted)
  "Refresh SUBDIRS, then run CALLBACK in this buffer, optionally SHOW-OMITTED."
  (tramp-async-files--pending-a)
  (let* ((buffer (current-buffer)) (state tramp-async-files--state)
         (directory (tramp-async-files--state-file state))
         (initial (not (derived-mode-p 'dired-mode))) job)
    (when (derived-mode-p 'wdired-mode) (user-error "Finish editing directory names first"))
    (tramp-async-files--run-hook 'dired-before-readin-hook)
    (setq job
          (tramp-async-call
           directory 'tramp-async-files-worker-dired
           (list directory (if initial dired-listing-switches dired-actual-switches)
                 tramp-async-files-size-limit (tramp-async-files--locals-enabled-p)
                 (or subdirs (and (not initial)
                                  (remove directory (mapcar #'car dired-subdir-alist))))
                 (when (boundp 'dired-subtree-overlays)
                   (mapcar (lambda (ov) (overlay-get ov 'dired-subtree-name)) dired-subtree-overlays))
                 (tramp-async-files--envrc-executable))
           (lambda (result)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (eq job (tramp-async-files--state-job state))
                   (setf (tramp-async-files--state-job state) nil)
                   (if (not (eq (plist-get result :status) 'ok))
                       (tramp-async-files--failure state result)
                     (condition-case err
                         (if (and (not initial) (not (eq major-mode 'dired-mode)))
                             (error "Directory mode changed while reading; contents preserved")
                           (let ((dired-omit-mode (and (not show-omitted) (bound-and-true-p dired-omit-mode))))
                             (tramp-async-dired--install (plist-get result :value) initial)
                             (when callback
                               (tramp-async-files--without-remote-io (funcall callback)))))
                       (error (tramp-async-files--failure state (list :error err)))))))))
           :library nil :mutating (and (tramp-async-files--envrc-executable) t)))
    (setf (tramp-async-files--state-job state) job
          (tramp-async-files--state-phase state) 'opening)
    (force-mode-line-update)))

(defun tramp-async-dired--revert (&optional _ignore-auto _noconfirm)
  "Request a fresh worker listing for the current directory."
  (tramp-async-dired--request))

(defun tramp-async-dired (directory &optional switches display callback)
  "Visit DIRECTORY with SWITCHES and DISPLAY, then run CALLBACK in its buffer."
  (interactive (tramp-async-files--read-args))
  (setq directory (file-name-as-directory (tramp-async-dired--expand directory)))
  (let* ((existing (cl-find-if
                    (lambda (buffer)
                      (with-current-buffer buffer
                        (and (tramp-async-dired--managed-p)
                             (equal (tramp-async-dired--expand directory) default-directory))))
                    (buffer-list)))
         (buffer (or existing (generate-new-buffer (file-name-nondirectory (directory-file-name directory))))))
    (with-current-buffer buffer
      (unless existing
        (setq default-directory directory buffer-read-only t
              tramp-async-files--state (tramp-async-files--state-create :file directory))
        (tramp-async-file-mode 1))
      (when (or switches (not existing))
        (let ((dired-listing-switches (or switches dired-listing-switches)))
          (when existing (setq dired-actual-switches dired-listing-switches))
          (tramp-async-dired--request nil callback)))
      (when (and existing (not switches) callback)
        (tramp-async-files--pending-a)
        (tramp-async-files--without-remote-io (funcall callback))))
    (funcall (or display #'pop-to-buffer-same-window) buffer)
    buffer))

(defmacro tramp-async-dired--entry (name display)
  "Define NAME as interactive Dired advice using DISPLAY."
  `(defun ,name (original directory &optional switches)
     "Adapt remote interactive Dired, preserving ORIGINAL for Lisp calls."
     (interactive (append (tramp-async-files--read-args)
                          (list (and current-prefix-arg (read-string "Dired switches: " dired-listing-switches)))))
     (if (and (called-interactively-p 'any)
              (stringp directory) (file-remote-p (tramp-async-files--expand directory) nil 'never))
         (tramp-async-dired directory switches ,display)
       (funcall original directory switches))))
(tramp-async-dired--entry tramp-async-dired--entry-a #'pop-to-buffer-same-window)
(tramp-async-dired--entry tramp-async-dired--window-a #'switch-to-buffer-other-window)
(tramp-async-dired--entry tramp-async-dired--frame-a #'switch-to-buffer-other-frame)

(defun tramp-async-dired--visit (display)
  "Visit the entry at point through the worker, using DISPLAY."
  (let ((file (dired-get-filename nil t)))
    (unless file (user-error "No file on this line"))
    (let* ((previous (current-buffer))
           (replace (and dired-kill-when-opening-new-dired-buffer
                         (eq display #'pop-to-buffer-same-window)
                         (< (length (get-buffer-window-list previous nil t)) 2)))
           (buffer (tramp-async-files--visit file display nil t)))
      (when (and replace (not (eq buffer previous)))
        (with-current-buffer buffer
          (if (or (tramp-async-dired--managed-p) (tramp-async-dired--remote-p)) (kill-buffer previous)
            (setq tramp-async-dired--previous-buffer previous))))
      buffer)))

(defun tramp-async-dired--find-a (original &rest args)
  "Adapt Dired entry visits, delegating unmanaged buffers to ORIGINAL ARGS."
  (if (and (called-interactively-p 'any)
           (or (tramp-async-dired--managed-p) (tramp-async-dired--remote-p)))
      (tramp-async-dired--visit #'pop-to-buffer-same-window)
    (apply original args)))
(defun tramp-async-dired--find-window-a (original &rest args)
  "Adapt Dired other-window visits, otherwise invoke ORIGINAL with ARGS."
  (if (and (called-interactively-p 'any)
           (or (tramp-async-dired--managed-p) (tramp-async-dired--remote-p)))
      (tramp-async-dired--visit #'switch-to-buffer-other-window)
    (apply original args)))
(defun tramp-async-dired--up-a (original &optional other-window)
  "Visit the parent asynchronously, honoring OTHER-WINDOW or ORIGINAL."
  (if (and (called-interactively-p 'any)
           (or (tramp-async-dired--managed-p) (tramp-async-dired--remote-p)))
      (progn
       (tramp-async-dired--adopt)
       (let* ((dir (file-name-as-directory (tramp-async-dired--expand (dired-current-directory))))
             (local (file-remote-p dir 'localname 'never)))
        (cond
         ;; A root has no parent on this host.  Do not cross the TRAMP prefix.
         ((equal local "/") (message "Already at remote root"))
         ((and local (string-prefix-p "~" local))
          ;; An adopted buffer may not yet have a resolved home directory.
          ;; Keep /.. intact until the worker has expanded the remote home.
          (tramp-async-dired (concat dir "../") nil
                             (if other-window #'switch-to-buffer-other-window #'pop-to-buffer-same-window)
                             (lambda () (tramp-async-dired--locate dir))))
         (t
          (let ((up (file-name-directory (directory-file-name dir))))
            (or (dired-goto-file (directory-file-name dir))
                (and (cdr dired-subdir-alist) (dired-goto-subdir up))
                (tramp-async-dired up nil
                                   (if other-window #'switch-to-buffer-other-window #'pop-to-buffer-same-window)
                                   (lambda () (tramp-async-dired--locate dir)))))))))
    (funcall original other-window)))
(defun tramp-async-dired--revert-a (original &rest args)
  "Adapt explicit dired-revert calls, otherwise apply ORIGINAL to ARGS."
  (if (and (called-interactively-p 'any)
           (or (tramp-async-dired--managed-p) (tramp-async-dired--remote-p))) (progn (tramp-async-dired--adopt) (tramp-async-dired--revert))
    (apply original args)))
(defun tramp-async-dired--insert-a (original directory &optional switches no-error)
  "Insert DIRECTORY asynchronously, otherwise invoke ORIGINAL.
SWITCHES must match the current listing; NO-ERROR belongs to native Dired."
  (if (and (called-interactively-p 'any)
           (or (tramp-async-dired--managed-p) (tramp-async-dired--remote-p)))
      (progn
        (tramp-async-dired--adopt)
        (when (and switches (not (equal switches dired-actual-switches)))
          (user-error "Use the current Dired switches for asynchronous subdirectories"))
        (let ((name (file-name-as-directory (tramp-async-dired--expand directory)))
              (origin (point)))
          (tramp-async-dired--request
           (delete-dups (cons name (remove default-directory (mapcar #'car dired-subdir-alist))))
           (lambda ()
             (unless (dired-goto-subdir (file-name-as-directory (tramp-async-dired--expand name)))
               (error "Inserted directory is missing from the listing: %s" name))
             (push-mark (min origin (point-max)))))))
    (funcall original directory switches no-error)))

(defun tramp-async-dired--goto-a (original file)
  "Find FILE synchronously in managed listings, preserving ORIGINAL elsewhere."
  (if (not (tramp-async-dired--managed-p)) (funcall original file)
    (unless (file-name-absolute-p file) (error "File name `%s' is not absolute" file))
    (tramp-async-files--without-remote-io
      (let ((file (directory-file-name (tramp-async-dired--expand file))))
        (let ((position
               (save-excursion
                 (goto-char (point-min))
                 (catch 'found
                   (while (not (eobp))
                     (when-let* ((entry (dired-get-filename nil t)))
                      (when (equal file (directory-file-name entry))
                       (dired-move-to-filename)
                       (throw 'found (point))))
                     (forward-line 1))))))
          (when position (goto-char position)))))))

(defun tramp-async-dired--maybe-insert-a (original directory &optional switches no-error)
  "Reuse an inserted DIRECTORY or fetch it; preserve ORIGINAL's Lisp contract."
  (if (not (and (called-interactively-p 'any) (tramp-async-dired--remote-p)))
      (funcall original directory switches no-error)
    (tramp-async-dired--adopt)
    (let ((name (file-name-as-directory (tramp-async-dired--expand directory)))
          (origin (point)))
      (if (and (not switches) (dired-goto-subdir name))
          (progn
            (unless (dired-subdir-hidden-p name) (dired-initial-position name))
            (push-mark origin))
        (funcall-interactively #'dired-insert-subdir directory switches no-error)))))

(defun tramp-async-dired--locate (file)
  "Locate FILE after a listing arrives, refreshing once to reveal omitted rows."
  (unless (dired-goto-file (tramp-async-dired--expand file))
    (tramp-async-dired--request
     nil (lambda () (dired-goto-file (tramp-async-dired--expand file))) t)))

(defun tramp-async-dired--mark-omitted-a (original)
  "Refresh before marking omitted files interactively; preserve ORIGINAL otherwise."
  (if (not (and (called-interactively-p 'any) (tramp-async-dired--remote-p)))
      (funcall original)
    (tramp-async-dired--adopt)
    (tramp-async-dired--request
     nil (lambda ()
           (dired-mark-unmarked-files (dired-omit-regexp) nil nil dired-omit-localp
                                     (dired-omit-case-fold-p default-directory))) t)))

(defun tramp-async-dired--jump-a (original &optional other-window file-name)
  "Complete an interactive Dired jump after loading; keep ORIGINAL for Lisp."
  (if (not (and (called-interactively-p 'any)
                (file-remote-p (tramp-async-dired--expand (or file-name buffer-file-name default-directory)) nil 'never)))
      (funcall original other-window file-name)
    (cond
     ((and (derived-mode-p 'dired-mode) (null file-name))
      (funcall-interactively #'dired-up-directory other-window))
     ((not (or file-name buffer-file-name))
      (tramp-async-dired default-directory nil
                         (if other-window #'switch-to-buffer-other-window #'pop-to-buffer-same-window)))
     (t
      (let* ((file (tramp-async-dired--expand (or file-name buffer-file-name)))
             (directory (file-name-directory file)))
      (tramp-async-dired
       directory nil (if other-window #'switch-to-buffer-other-window #'pop-to-buffer-same-window)
       (lambda () (tramp-async-dired--locate file))))))))

(defun tramp-async-dired--jump-window-a (original &optional file-name)
  "Adapt an interactive other-window jump while preserving ORIGINAL's Lisp call."
  (if (and (called-interactively-p 'any)
           (file-remote-p (tramp-async-dired--expand (or file-name buffer-file-name default-directory)) nil 'never))
      ;; Enter through the interactive command so the whole workflow is adapted.
      (let ((current-prefix-arg nil))
        (funcall-interactively #'dired-jump t file-name))
    (funcall original file-name)))

(defun tramp-async-dired--subtree-toggle-a (original)
  "Adapt subtree expansion as a complete interactive command, preserving ORIGINAL."
  (if (and (called-interactively-p 'any)
           (or (tramp-async-dired--managed-p) (tramp-async-dired--remote-p))
           (not (dired-subtree--is-expanded-p)))
      (progn (tramp-async-dired--adopt) (call-interactively #'dired-subtree-insert))
    (funcall original)))

(defun tramp-async-dired--subtree-a (original)
  "Supply worker listings to ORIGINAL's subtree insertion code."
  (if (not (and (or (tramp-async-dired--managed-p) (tramp-async-dired--remote-p))
                (or tramp-async-dired--subtree-listings (called-interactively-p 'any))))
      (funcall original)
    (tramp-async-dired--adopt)
    (when (and (dired-subtree--dired-line-is-directory-or-link-p)
               (not (dired-subtree--is-expanded-p)))
      (let* ((dir (directory-file-name (tramp-async-dired--expand (dired-get-filename))))
             (cached (assoc dir tramp-async-dired--subtree-listings)))
        (if cached
            (tramp-async-files--without-remote-io
              ;; Use the extension's own parser and overlay logic.  Only its
              ;; insert-directory I/O boundary is replaced by fetched text.
              (let ((insert-directory-function (symbol-function 'insert-directory)))
                (cl-letf (((symbol-function 'insert-directory)
                           (lambda (file &rest args)
                             (if (equal (directory-file-name file) dir)
                                 (insert (cdr cached))
                               (apply insert-directory-function file args)))))
                  (funcall original))))
          (tramp-async-files--pending-a)
          (let ((buffer (current-buffer)) (state tramp-async-files--state) job)
            (setq job
                  (tramp-async-call
                   default-directory 'tramp-async-files-worker-subtree
                   (list dir dired-actual-switches tramp-async-files-size-limit)
                   (lambda (result)
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (when (eq job (tramp-async-files--state-job state))
                           (setf (tramp-async-files--state-job state) nil
                                 (tramp-async-files--state-phase state) nil)
                           (if (not (eq (plist-get result :status) 'ok))
                               (tramp-async-files--failure state result)
                             (condition-case err
                                 (save-excursion
                                   (let ((value (plist-get result :value))
                                         (snapshot (tramp-async-files--state-snapshot state)))
                                     (dolist (key '(:aliases :case-fold))
                                       (let ((entries (copy-tree (plist-get snapshot key))))
                                         (dolist (entry (plist-get value key))
                                           (setf (alist-get (car entry) entries nil nil #'equal) (cdr entry)))
                                         (setq snapshot (plist-put snapshot key entries))))
                                     (setf (tramp-async-files--state-snapshot state) snapshot)
                                     (when (and (eq major-mode 'dired-mode) (dired-goto-file dir))
                                       (let ((tramp-async-dired--subtree-listings
                                              (list (cons dir (plist-get value :text)))))
                                         (tramp-async-dired--subtree-a original)))))
                               (error (setq buffer-read-only t)
                                      (tramp-async-files--failure state (list :error err)))))))))
                   :library nil :mutating nil))
            (setf (tramp-async-files--state-job state) job
                  (tramp-async-files--state-phase state) 'opening)))))))

(defconst tramp-async-dired--advices
  '((dired . tramp-async-dired--entry-a)
    (dired-other-window . tramp-async-dired--window-a)
    (dired-other-frame . tramp-async-dired--frame-a)
    (dired-find-file . tramp-async-dired--find-a)
    (dired-find-file-other-window . tramp-async-dired--find-window-a)
    (dired-up-directory . tramp-async-dired--up-a)
    (dired-revert . tramp-async-dired--revert-a)
    (dired-do-redisplay . tramp-async-dired--revert-a)
    (dired-goto-file . tramp-async-dired--goto-a)
    (dired-insert-subdir . tramp-async-dired--insert-a)
    (dired-maybe-insert-subdir . tramp-async-dired--maybe-insert-a)
    (dired-mark-omitted . tramp-async-dired--mark-omitted-a)
    (dired-jump . tramp-async-dired--jump-a)
    (dired-jump-other-window . tramp-async-dired--jump-window-a)
    (dired-subtree-toggle . tramp-async-dired--subtree-toggle-a)
    (dired-subtree-insert . tramp-async-dired--subtree-a)))

(defun tramp-async-dired-integration (enable)
  "Install or remove the Dired command adapters according to ENABLE."
  ;; Retire the shared adapter when reloading an already enabled package.
  (advice-remove 'dired-maybe-insert-subdir #'tramp-async-dired--insert-a)
  (tramp-async-dired-ops-integration enable)
  (if enable
      (advice-add 'wdired-change-to-wdired-mode :before #'tramp-async-files--pending-a)
    (advice-remove 'wdired-change-to-wdired-mode #'tramp-async-files--pending-a))
  (dolist (entry tramp-async-dired--advices)
    (if enable (advice-add (car entry) :around (cdr entry))
      (advice-remove (car entry) (cdr entry)))))

;;; Batch saving

(require 'diff)

(cl-defstruct (tramp-async-batch--state (:constructor tramp-async-batch--create))
  buffers failures hooks arg callback timer)
(defvar tramp-async-batch--active nil)

(defun tramp-async-batch--diff (buffer)
  "Compare BUFFER with a worker-read disk snapshot, without remote diff I/O."
  (let* ((buffer (or buffer (current-buffer)))
         (file (buffer-file-name buffer)))
    (unless file (user-error "Buffer has no visited file"))
    (tramp-async-call
     (file-name-directory file) 'tramp-async-files-worker-visit
     (list file tramp-async-files-size-limit)
     (lambda (result)
       (if (not (eq (plist-get result :status) 'ok))
           (display-warning 'tramp-async-files (error-message-string (plist-get result :error)))
         (when (buffer-live-p buffer)
           (with-temp-buffer
             (let ((snapshot (current-buffer)) (default-directory temporary-file-directory))
               (insert (plist-get (plist-get result :value) :text))
               (let ((diff-buffer (diff-no-select snapshot buffer)))
                 (with-current-buffer diff-buffer
                   (setq-local revert-buffer-function
                               (lambda (&rest _) (tramp-async-batch--diff buffer))))
                 (pop-to-buffer diff-buffer)))))))
     :library nil :mutating nil)))

(defun tramp-async-batch--select (arg pred)
  "Select buffers using native ARG, PRED and query actions.
Do not rebind synchronous saving while the user answers prompts."
  (unless pred
    (setq pred (if (and (symbolp save-some-buffers-default-predicate)
                       (get save-some-buffers-default-predicate 'save-some-buffers-function))
                   (funcall save-some-buffers-default-predicate)
                 save-some-buffers-default-predicate)))
  (let* (selected switched
         (save-some-buffers--switch-window-callback (lambda (buffer) (setq switched buffer)))
         (actions
          (mapcar (lambda (entry)
                    (if (eq (car entry) ?d)
                        (list ?d (lambda (buffer)
                                   (if (file-remote-p (or (buffer-file-name buffer) "") nil 'never)
                                       (tramp-async-batch--diff buffer)
                                     (funcall (cadr entry) buffer))
                                   nil)
                              (nth 2 entry))
                      entry)) save-some-buffers-action-alist)))
    (unwind-protect
        (save-window-excursion
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (when (and buffer-save-without-query (buffer-modified-p))
                (push buffer selected))))
          (map-y-or-n-p
           (lambda (buffer)
             (if arg t
               (if (buffer-file-name buffer)
                   (format "Save file %s (buffer %s)? " (buffer-file-name buffer) (buffer-name buffer))
                 (format "Save buffer %s? " (buffer-name buffer)))))
           (lambda (buffer) (cl-pushnew buffer selected))
           (cl-set-difference (files--buffers-needing-to-be-saved pred) selected)
           '("buffer" "buffers" "save") actions))
      (when switched (pop-to-buffer-same-window switched)))
    (nreverse selected)))

(defun tramp-async-batch--finish (batch)
  "Report acknowledged BATCH outcomes, then run its completion callback."
  (when (timerp (tramp-async-batch--state-timer batch))
    (cancel-timer (tramp-async-batch--state-timer batch)))
  (setq tramp-async-batch--active nil)
  (let ((failed (tramp-async-batch--state-failures batch)) (saved 0) (changed 0))
    (dolist (buffer (tramp-async-batch--state-buffers batch))
      (cond
       ((not (buffer-live-p buffer)) (push (cons buffer "Buffer was killed") failed))
       ((assq buffer failed))
       (t
        (with-current-buffer buffer
          (cond
           ((and tramp-async-files--state (eq (tramp-async-files--state-phase tramp-async-files--state) 'error))
            (push (cons buffer (tramp-async-files--state-error tramp-async-files--state)) failed))
           ((buffer-modified-p) (cl-incf changed))
           (t (cl-incf saved)))))))
    (dolist (hook (tramp-async-batch--state-hooks batch))
      (condition-case err (funcall hook nil (tramp-async-batch--state-arg batch))
        (error (push (cons hook (error-message-string err)) failed))))
    (message "Save batch: %d saved, %d with newer edits, %d failed" saved changed (length failed))
    (when failed
      (display-warning 'tramp-async-files
                       (mapconcat (lambda (entry) (format "%s: %s" (car entry) (cdr entry))) failed "\n")))
    (when-let* ((callback (tramp-async-batch--state-callback batch)))
      (funcall callback (list :status (if (or failed (> changed 0)) 'error 'ok)
                             :saved saved :changed changed :failures failed)))))

(defun tramp-async-batch--poll (batch)
  "Finish BATCH when every selected buffer has settled."
  (when (eq batch tramp-async-batch--active)
    (unless (cl-some (lambda (buffer)
                       (and (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (or tramp-async-files--format-token
                                  (and tramp-async-files--state
                                       (or (tramp-async-files--state-job tramp-async-files--state)
                                           (eq (tramp-async-files--state-phase tramp-async-files--state) 'preparing)))))))
                     (tramp-async-batch--state-buffers batch))
      (tramp-async-batch--finish batch))))

(defun tramp-async-save-some-buffers (&optional arg pred callback)
  "Save selected buffers asynchronously, respecting ARG and PRED.
Selection uses `save-some-buffers'.  CALLBACK receives a plist with :status,
:saved, :changed and :failures after all selected saves have settled."
  (interactive "P")
  (when tramp-async-batch--active (user-error "A save batch is still pending"))
  (let ((batch (tramp-async-batch--create
                :buffers (tramp-async-batch--select arg pred)
                :hooks save-some-buffers-functions :arg arg :callback callback)))
    (setq tramp-async-batch--active batch)
    (dolist (buffer (tramp-async-batch--state-buffers batch))
      (condition-case err
          (with-current-buffer buffer
            (if (and buffer-file-name (or tramp-async-file-mode (file-remote-p buffer-file-name)))
                (tramp-async-files-save-buffer)
              (save-buffer)))
        ((error quit)
         (push (cons buffer (error-message-string err)) (tramp-async-batch--state-failures batch)))))
    (setf (tramp-async-batch--state-timer batch)
          (run-at-time 0.05 0.05 #'tramp-async-batch--poll batch))
    batch))

(defun tramp-async-batch--save-a (original &optional arg pred)
  "Adapt interactive batch saving; ORIGINAL handles synchronous Lisp calls."
  (interactive "P")
  (if (called-interactively-p 'any)
      (tramp-async-save-some-buffers arg pred)
    (funcall original arg pred)))

(defun tramp-async-batch--exit (original args)
  "Suspend ORIGINAL with ARGS at its native save pass, then resume on success.
This also preserves emacsclient buffer selection and native exit dialogs."
  (when tramp-async-batch--active (user-error "A save batch is still pending"))
  (let ((frame (selected-frame)) request)
    (catch 'tramp-async-batch--deferred
      (cl-letf (((symbol-function 'save-some-buffers)
                 (lambda (&optional arg pred)
                   (setq request (list arg pred))
                   (throw 'tramp-async-batch--deferred nil))))
        (apply original args)))
    (when request
      (tramp-async-save-some-buffers
       (car request) (cadr request)
       (lambda (result)
         (if (not (eq (plist-get result :status) 'ok))
             (message "Exit cancelled: selected buffers still need attention")
           (when (frame-live-p frame)
             (with-selected-frame frame
               ;; Preserve native remaining-buffer/process/exit queries.
               (cl-letf (((symbol-function 'save-some-buffers) #'ignore))
                 (apply original args))))))))))

(defun tramp-async-batch--exit-a (original &optional arg restart)
  "Await interactive saves before ORIGINAL's exit with ARG and RESTART."
  (interactive "P")
  (if (called-interactively-p 'any)
      (tramp-async-batch--exit original (list arg restart))
    (funcall original arg restart)))

(defun tramp-async-batch--terminal-a (original &optional arg)
  "Await interactive saves before ORIGINAL's terminal exit with ARG."
  (interactive "P")
  (if (called-interactively-p 'any)
      (tramp-async-batch--exit original (list arg))
    (funcall original arg)))

(defun tramp-async-batch--exit-query-h ()
  "Prevent exiting while a batch still has work or completion hooks."
  (if tramp-async-batch--active
      (progn (message "Save batch still pending") nil)
    t))

(defun tramp-async-batch-integration (enable)
  "Install or remove interactive batch saving according to ENABLE."
  (dolist (entry '((save-some-buffers . tramp-async-batch--save-a)
                   (save-buffers-kill-emacs . tramp-async-batch--exit-a)
                   (save-buffers-kill-terminal . tramp-async-batch--terminal-a)))
    (if enable (advice-add (car entry) :around (cdr entry))
      (advice-remove (car entry) (cdr entry))))
  (if enable (add-hook 'kill-emacs-query-functions #'tramp-async-batch--exit-query-h)
    (remove-hook 'kill-emacs-query-functions #'tramp-async-batch--exit-query-h)
    (when tramp-async-batch--active
      (cancel-timer (tramp-async-batch--state-timer tramp-async-batch--active))
      ;; Never continue an exit after the integration was disabled.
      (setq tramp-async-batch--active nil))))


(provide 'tramp-async-files)
(provide 'tramp-async-dired-ops)
(provide 'tramp-async-dired)
(provide 'tramp-async-batch)
;;; tramp-async-files.el ends here
