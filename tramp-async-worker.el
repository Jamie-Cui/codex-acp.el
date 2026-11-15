;;; tramp-async-worker.el --- Bounded worker execution and IPC -*- lexical-binding: t -*-

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
;; Bounded worker execution and IPC.
;;; Code:
(require 'tramp-async)

;;; Child worker


(require 'cl-lib)
(require 'subr-x)
(require 'async)
(require 'tramp)

(defvar tramp-async-worker--request nil)
(defvar tramp-async-worker--mutating nil)
(defvar tramp-async-worker--libraries nil)

(defun tramp-async-worker--send (&rest arguments)
  "Send protocol ARGUMENTS with Unicode escaped for async.el's message reader."
  ;; async.el decodes final values, but its intermediate-message reader reads
  ;; raw base64 bytes.  ASCII Lisp escapes preserve multibyte strings there.
  (let ((print-escape-multibyte t)) (apply #'async-send arguments)))

(define-error 'tramp-async-path-outside-roots "Path is outside the allowed roots" 'file-error)

(defun tramp-async-worker-path (path &optional roots keep-name)
  "Resolve PATH on the current host, optionally confined to canonical ROOTS.
Return a canonical full filename, or the expanded name when KEEP-NAME is set.
ROOTS nil permits any path on this host.
Signal `tramp-async-path-outside-roots' with the canonical target on denial.
Path checks are not an OS sandbox and cannot prevent external symlink races."
  (unless (and (stringp path) (not (string-empty-p path))
               (not (string-match-p "[[:cntrl:]]" path)))
    (error "Expected a nonempty file path"))
  (let* ((prefix (file-remote-p default-directory))
         (remote (file-remote-p path))
         (local (if remote (tramp-file-name-localname (tramp-dissect-file-name path)) path))
         (target (if prefix
                     (concat (or remote prefix)
                             (if (string-prefix-p "~" local) local
                               (expand-file-name local (file-local-name default-directory))))
                   (expand-file-name path default-directory))))
    (unless (equal (file-remote-p target) prefix)
      (error "Cross-host file access is not supported"))
    (let ((true (file-truename target)))
      (when roots
        (unless (cl-some
                 (lambda (root)
                   (let ((true-root (file-name-as-directory (file-truename root))))
                     (or (equal (directory-file-name true) (directory-file-name true-root))
                         (string-prefix-p true-root true)))) roots)
          (signal 'tramp-async-path-outside-roots (list true))))
      (if keep-name (expand-file-name target) true))))

(defvar tramp-async-worker--event-serial 0)

(defun tramp-async-worker-request (payload)
  "Send adapter PAYLOAD to this request's local handler and return its reply.
Only explicitly opted-in requests support events.  No code is evaluated."
  (unless (plist-get tramp-async-worker--request :events)
    (error "Adapter events were not enabled for this request"))
  (let ((id (plist-get tramp-async-worker--request :id))
        (token (cl-incf tramp-async-worker--event-serial)))
    (tramp-async-worker--send :event 'adapter-event :id id :token token :payload payload)
    (let ((reply (async-receive)))
      (unless (and (eq (plist-get reply :event) 'adapter-reply)
                   (equal id (plist-get reply :id)) (equal token (plist-get reply :token)))
        (error "Invalid adapter reply"))
      (when-let* ((err (plist-get reply :error))) (signal (car err) (cdr err)))
      (plist-get reply :value))))

(defun tramp-async-worker--check-write (path)
  "Ask the parent to protect visiting buffers before writing canonical PATH."
  (unless tramp-async-worker--request (error "Writes require a tramp-async worker request"))
  (let (aliases)
    (dolist (file (plist-get tramp-async-worker--request :visiting))
      (when (and (equal (file-remote-p file) (file-remote-p path))
                 (or (equal (file-truename file) path) (file-equal-p file path)))
        (push file aliases)))
    (tramp-async-worker--send :event 'check-write :id (plist-get tramp-async-worker--request :id)
                :path path :aliases aliases)
    (let ((decision (async-receive)))
      (unless (and (eq (plist-get decision :event) 'write-decision)
                   (equal (plist-get decision :id) (plist-get tramp-async-worker--request :id)))
        (error "Invalid write decision"))
      (when-let* ((err (plist-get decision :error))) (signal (car err) (cdr err))))))

(defun tramp-async-worker-write-buffer (path &optional max-bytes)
  "Write the current buffer to PATH after checking visiting buffers.
MAX-BYTES defaults to 4 MiB.  Preserve ordinary `write-region' semantics,
including hard links and file metadata.  This is not an atomic replacement;
an interrupted transfer can have an unknown outcome and must not be retried
automatically.  Coding follows `buffer-file-coding-system'."
  (when (> (string-bytes (encode-coding-string (buffer-string) buffer-file-coding-system))
           (or max-bytes (* 4 1024 1024)))
    (error "New file content exceeds the file size limit"))
  (setq path (file-truename path))
  (tramp-async-worker--check-write path)
  (let ((coding-system-for-write buffer-file-coding-system))
    (setq tramp-async-worker--mutating t)
    (write-region (point-min) (point-max) path nil 'silent)))

(defun tramp-async-worker--read (path max-bytes)
  "Insert regular file PATH, limiting its size to MAX-BYTES."
  (unless (file-regular-p path) (error "Not a regular file: %s" path))
  (let ((attributes (file-attributes path)))
    (unless (and attributes (<= (file-attribute-size attributes) max-bytes))
      (error "File exceeds the file size limit")))
  ;; Read one extra byte to detect growth since stat without loading an
  ;; arbitrarily large file.  Decoding a bounded prefix may still fail.
  (insert-file-contents path nil 0 (1+ max-bytes))
  (when (or (> (buffer-size) max-bytes)
            (> (file-attribute-size (file-attributes path)) max-bytes))
    (error "File grew beyond the file size limit")))

(defun tramp-async-worker--command (args)
  "Run ARGS's literal :argv with bounded output and return process metadata."
  (let* ((argv (plist-get args :argv))
         (default-directory (file-name-as-directory
                             (tramp-async-worker-path (or (plist-get args :cwd) ".")
                                                       (plist-get args :roots))))
         (limit (or (plist-get args :max-chars) 65536))
         (timeout (or (plist-get args :command-timeout) 120))
         (deadline (+ (float-time) timeout))
         (output "") truncated process)
    (unless (and (consp argv) (cl-every (lambda (arg) (and (stringp arg)
                                                        (not (string-match-p "\0" arg)))) argv)
                 (not (string-empty-p (car argv)))
                 (integerp limit) (> limit 0) (numberp timeout) (> timeout 0))
      (error "Invalid command argv or limits"))
    (unwind-protect
        (progn
          (setq tramp-async-worker--mutating t
                process
                (make-process
                 :name "tramp-async-command" :command argv :file-handler t
                 :connection-type 'pipe :noquery t :coding 'utf-8-unix
                 :sentinel #'ignore
                 :filter (lambda (_process chunk)
                           (let ((room (- limit (length output))))
                             (when (> (length chunk) room) (setq truncated t))
                             (setq output (concat output (substring chunk 0 (min room (length chunk)))))))))
          (unless (processp process) (error "This TRAMP method cannot start a process"))
          (while (and (process-live-p process) (< (float-time) deadline))
            (accept-process-output process 0.05))
          (if (process-live-p process)
              (error "Command timed out; remote side effects may have occurred")
            (list :exit (process-exit-status process) :status (process-status process)
                  :output output :truncated truncated)))
      (when (and (processp process) (process-live-p process)) (delete-process process)))))

(defun tramp-async-worker-run (operation args)
  "Execute OPERATION with plist ARGS; called through `tramp-async-submit'.
File operations use :path and optional :roots (allowed canonical roots).
read-file returns text; :line/:lines select lines, :max-bytes defaults to
4 MiB, :max-chars to 65536.  write-file uses :text and optional :coding
(default utf-8-unix); edit-file replaces a unique literal :old with :new.
Writes return the canonical filename.  file-truename returns a filename;
file-attributes returns the native attribute list (or nil when absent).
directory-files returns names; :full includes paths; :max-entries defaults
to 10000.  process-file uses :argv, :cwd, :max-chars, :command-timeout and
returns (:status exit/signal :exit CODE :output STRING :truncated BOOLEAN)."
  (if (eq operation 'process-file)
      (tramp-async-worker--command args)
    (let* ((path (tramp-async-worker-path (plist-get args :path) (plist-get args :roots)
                                          (eq operation 'file-attributes)))
           (max-bytes (or (plist-get args :max-bytes) (* 4 1024 1024))))
      (unless (and (integerp max-bytes) (> max-bytes 0)) (error "Invalid file size limit"))
      (pcase operation
        ('file-truename path)
        ('file-attributes (file-attributes path 'integer))
        ('directory-files
         (let* ((limit (or (plist-get args :max-entries) 10000))
                (files (progn
                         (unless (and (integerp limit) (> limit 0)) (error "Invalid directory limit"))
                         (directory-files path (plist-get args :full)
                                          directory-files-no-dot-files-regexp nil (1+ limit)))))
           (when (> (length files) limit) (error "Directory exceeds the entry limit"))
           files))
        ('read-file
         (let ((line (or (plist-get args :line) 1))
               (lines (plist-get args :lines))
               (chars (or (plist-get args :max-chars) 65536)))
           (unless (and (integerp line) (> line 0) (integerp chars) (> chars 0)
                        (or (null lines) (and (integerp lines) (> lines 0))))
             (error "Invalid read range"))
           (with-temp-buffer
             (tramp-async-worker--read path max-bytes)
             (goto-char (point-min)) (forward-line (1- line))
             (let ((start (point)))
               (if lines (forward-line lines) (goto-char (point-max)))
               (buffer-substring-no-properties start (min (point) (+ start chars)))))))
        ((or 'write-file 'edit-file)
         (with-temp-buffer
           (if (eq operation 'write-file)
               (progn
                 (set-buffer-file-coding-system (or (plist-get args :coding) 'utf-8-unix))
                 (insert (or (plist-get args :text) (error "Missing write text"))))
             (tramp-async-worker--read path max-bytes)
             (let ((old (plist-get args :old)) (new (plist-get args :new)) (case-fold-search nil))
               (unless (and (stringp old) (not (string-empty-p old)) (stringp new))
                 (error "Expected nonempty old text and replacement text"))
               (goto-char (point-min))
               (unless (search-forward old nil t) (error "Old text not found"))
               (let ((end (point)))
                 (when (search-forward old nil t) (error "Old text occurs more than once"))
                 (delete-region (- end (length old)) end)
                 (goto-char (- end (length old))) (insert new))))
           (tramp-async-worker-write-buffer path max-bytes))
         path)
        (_ (error "Unknown asynchronous operation: %s" operation))))))

(defun tramp-async-worker--no-prompt (&rest _)
  "Reject interactive authentication in this protocol-only worker."
  (error "Worker needs interactive input; configure noninteractive authentication"))

(defun tramp-async-worker-main (settings)
  "Serve serial requests until stdin closes, applying explicit SETTINGS."
  (dolist (setting settings) (set (car setting) (cdr setting)))
  (setq tramp-persistency-file-name nil)
  ;; Authentication must never consume async.el's protocol input.
  (dolist (function '(read-passwd read-string y-or-n-p yes-or-no-p))
    (fset function #'tramp-async-worker--no-prompt))
  (tramp-async-worker--send :event 'ready)
  (condition-case nil
      (while t
        (let* ((tramp-async-worker--request (async-receive))
               (default-directory (file-name-as-directory
                                   (plist-get tramp-async-worker--request :directory)))
               (tramp-async-worker--mutating nil)
               (remote-file-name-inhibit-cache t)
               (result
                (condition-case err
                    (progn
                      (when-let* ((library (plist-get tramp-async-worker--request :library)))
                        (unless (member library tramp-async-worker--libraries)
                          (load library nil t)
                          (push library tramp-async-worker--libraries)))
                      (list :status 'ok
                            :value (apply (plist-get tramp-async-worker--request :function)
                                          (plist-get tramp-async-worker--request :arguments))))
                  ((error quit) (list :status 'error :error err
                                      :uncertain tramp-async-worker--mutating)))))
          (tramp-async-worker--send :event 'result :id (plist-get tramp-async-worker--request :id) :result result)))
    (end-of-file nil)))


(provide 'tramp-async-worker)
;;; tramp-async-worker.el ends here
