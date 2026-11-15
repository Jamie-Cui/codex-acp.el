;;; tramp-async.el --- Run remote I/O in persistent child processes -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Assisted-by: Codex:GPT-6
;; Maintainer: Jamie Cui <jamie.cui@outlook.com>
;; Version: 0.4.0
;; URL: https://github.com/Jamie-Cui/codex-acp.el
;; Keywords: tools, processes
;; Package-Requires: ((emacs "30.1") (async "1.9.9"))
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
;; `tramp-async-submit' runs read-file, write-file, edit-file, directory-files,
;; file-attributes, file-truename and process-file in a local child Emacs.
;; `tramp-async-call' also runs a trusted named function from a local library.
;; Arguments and results must be readable Lisp data, never buffers/processes.
;; Callbacks receive (:status ok :value VALUE) or
;; (:status error/cancelled/timeout :error CONDITION :uncertain BOOLEAN).
;; Callbacks always run later, outside process filters.  There is no sync wait.
;;
;; Each full TRAMP connection has independent file and command workers.
;; Jobs within each lane run FIFO; long commands do not block file requests.
;; Local paths use the same two lanes.  File writes stay serialized; commands
;; may change files concurrently, so callers must coordinate such dependencies.
;; Cancelling an active job retires its worker, without replaying that job.
;; Remote side effects of an interrupted write/command may have happened.
;; Writes reject modified visiting buffers and guard matching buffers against
;; edits until completion; they do not revert buffers or mark them saved.
;;
;; Workers start without the interactive init file or its credential caches.
;; Use noninteractive SSH authentication.  Explicit TRAMP settings can be
;; supplied through `tramp-async-worker-settings'.  No remote agent is needed.
;; This is an explicit asynchronous API, not advice around synchronous APIs.
;; Loading the library starts no workers.  `tramp-async-shutdown' releases all.
;;
;; Example (the callback runs in the parent):
;; (tramp-async-submit "/ssh:user@host:/project/" 'read-file
;;                     '(:path "src/main.c" :line 1 :lines 100)
;;                     (lambda (result) (message "%S" result)))
;; Use :roots '("/ssh:user@host:/project/") to confine a file operation.
;; A process-file request takes :argv '("git" "status" "--short") and :cwd.
;; Process results include an exit code; a nonzero code is not a transport
;; error.  Result text is bounded; avoid large return values in custom jobs.
;; `tramp-async-files-mode' adapts interactive file visiting and saving.
;; Automatic envrc discovery/export is prepared before buffer initialization.
;; Native synchronous Lisp APIs keep their completion contracts.  Extensions
;; that need asynchronous work must use an explicit command adapter.
;; Cached metadata contains observed values only.  Hooks requiring uncached
;; remote I/O fail visibly and need their own command adapter.  Direct Lisp
;; calls and explicit envrc reloads still have native synchronous behavior.

;; Core worker execution lives in tramp-async-worker.el.  File snapshots and
;; interactive integration are optional libraries, loaded by their entry points.

;;; Code:
;;; Core API

;; Optional worker/UI features load only when called.
(autoload 'tramp-async-worker-path "tramp-async-worker" nil nil)
(autoload 'tramp-async-worker-request "tramp-async-worker" nil nil)
(autoload 'tramp-async-worker-write-buffer "tramp-async-worker" nil nil)
(autoload 'tramp-async-worker-run "tramp-async-worker" nil nil)
(autoload 'tramp-async-worker-main "tramp-async-worker" nil nil)
(autoload 'tramp-async-files-worker-context "tramp-async-files-worker" nil nil)
(autoload 'tramp-async-files-worker-visit "tramp-async-files-worker" nil nil)
(autoload 'tramp-async-files-worker-save "tramp-async-files-worker" nil nil)
(autoload 'tramp-async-files-worker-completions "tramp-async-files-worker" nil nil)
(autoload 'tramp-async-files-worker-locals "tramp-async-files-worker" nil nil)
(autoload 'tramp-async-files-worker-dired "tramp-async-files-worker" nil nil)
(autoload 'tramp-async-files-worker-subtree "tramp-async-files-worker" nil nil)
(autoload 'tramp-async-files-worker-listing "tramp-async-files-worker" nil nil)
(autoload 'tramp-async-files-worker-envrc "tramp-async-files-worker" nil nil)
(autoload 'tramp-async-dired-ops-worker-run "tramp-async-files-worker" nil nil)
(autoload 'tramp-async-file-mode "tramp-async-files" nil t)
(autoload 'tramp-async-files-cancel "tramp-async-files" nil t)
(autoload 'tramp-async-files-recover "tramp-async-files" nil t)
(autoload 'tramp-async-files-refresh-dir-locals "tramp-async-files" nil t)
(autoload 'tramp-async-files-find-file "tramp-async-files" nil t)
(autoload 'tramp-async-files-revert-buffer "tramp-async-files" nil t)
(autoload 'tramp-async-files-save-buffer "tramp-async-files" nil t)
(autoload 'tramp-async-files-mode "tramp-async-files" nil t)
(autoload 'tramp-async-files-unload-function "tramp-async-files" nil nil)
(autoload 'tramp-async-dired-operate "tramp-async-files" nil nil)
(autoload 'tramp-async-dired-ops-integration "tramp-async-files" nil nil)
(autoload 'tramp-async-dired "tramp-async-files" nil t)
(autoload 'tramp-async-dired-integration "tramp-async-files" nil nil)
(autoload 'tramp-async-save-some-buffers "tramp-async-files" nil t)
(autoload 'tramp-async-batch-integration "tramp-async-files" nil nil)


(require 'cl-lib)
(require 'async)
(require 'tramp)

(defgroup tramp-async nil "Remote I/O in child Emacs processes." :group 'tramp)
(defcustom tramp-async-timeout 120
  "Seconds allowed from submission, including queueing and worker startup."
  :type 'number :group 'tramp-async)
(defcustom tramp-async-idle-timeout 60
  "Seconds to retain an idle worker and its TRAMP connections."
  :type 'number :group 'tramp-async)
(defcustom tramp-async-queue-limit 64
  "Maximum pending jobs per lane of a connection."
  :type 'integer :group 'tramp-async)
(defcustom tramp-async-worker-settings nil
  "Alist of explicit variable values to set in each fresh worker.
Values must be readable Lisp data.  Do not put credentials here.  Workers
inherit the process environment, but not the interactive Emacs init file."
  :type '(alist :key-type symbol :value-type sexp) :group 'tramp-async)

(defconst tramp-async--directory
  (file-name-directory (or load-file-name buffer-file-name)))
(defvar tramp-async--workers (make-hash-table :test #'equal))
(defvar tramp-async--serial 0)
(cl-defstruct (tramp-async--worker (:constructor tramp-async--worker-create))
  key process ready active queue idle)
(cl-defstruct (tramp-async-request (:constructor tramp-async--request-create))
  "Handle returned by `tramp-async-call'; cancel with `tramp-async-cancel'."
  id worker directory function arguments library callback timeout mutating
  timer locks done write-buffer event-handler deadline rpc)

(defun tramp-async--callback (request result)
  "Deliver RESULT to REQUEST's callback at the isolation boundary."
  (condition-case err
      (funcall (tramp-async-request-callback request) result)
    (error (display-warning 'tramp-async (error-message-string err)))))

(defun tramp-async--settle (request result)
  "Complete REQUEST once with RESULT, releasing timers and buffer guards."
  (unless (tramp-async-request-done request)
    (setf (tramp-async-request-done request) t)
    (when (timerp (tramp-async-request-timer request))
      (cancel-timer (tramp-async-request-timer request)))
    (dolist (entry (tramp-async-request-locks request))
      (when (buffer-live-p (car entry))
        (with-current-buffer (car entry)
          (remove-hook 'before-change-functions (cdr entry) t))))
    (setf (tramp-async-request-locks request) nil)
    (run-at-time 0 nil #'tramp-async--callback request result)))

(defun tramp-async--retire (worker)
  "Stop WORKER's process and discard its connection state."
  (when (timerp (tramp-async--worker-idle worker))
    (cancel-timer (tramp-async--worker-idle worker)))
  (let ((process (tramp-async--worker-process worker)))
    (setf (tramp-async--worker-process worker) nil
          (tramp-async--worker-ready worker) nil
          (tramp-async--worker-idle worker) nil)
    (when (processp process)
      (set-process-sentinel process #'ignore)
      (set-process-filter process #'ignore)
      ;; Give batch Emacs a chance to clean up its own SSH/command children.
      ;; If it cannot exit, a bounded fallback still releases local resources.
      (when (process-live-p process)
        (signal-process (process-id process) 'SIGTERM)
        (run-at-time 1 nil (lambda ()
                             (when (process-live-p process) (delete-process process)))))
      (when (buffer-live-p (process-buffer process))
        (kill-buffer (process-buffer process))))))

(defun tramp-async--abort (request status message)
  "Abort REQUEST with STATUS and MESSAGE; never retry active work."
  (unless (tramp-async-request-done request)
    (let* ((worker (tramp-async-request-worker request))
           (active (eq request (tramp-async--worker-active worker))))
      (if active
          (progn
            (setf (tramp-async--worker-active worker) nil)
            (tramp-async--retire worker))
        (setf (tramp-async--worker-queue worker)
              (delq request (tramp-async--worker-queue worker))))
      (tramp-async--settle
       request (list :status status :error (list 'error message)
                     :uncertain (and active (tramp-async-request-mutating request))))
      (run-at-time 0 nil #'tramp-async--pump worker))))

(defun tramp-async-cancel (request)
  "Cancel REQUEST.  Active mutations may have an unknown remote outcome."
  (tramp-async--abort request 'cancelled "Job cancelled"))

(defvar tramp-async-before-buffer-write-hook nil
  "Hook run in each affected visiting buffer before a worker write.
Handlers may signal an error to reject a conflicting operation.  They must
inspect only in-memory state and must not perform synchronous remote I/O.")

(defun tramp-async--guard-write (request message)
  "Check visiting buffers for REQUEST's write described by MESSAGE."
  (let ((paths (cons (plist-get message :path) (plist-get message :aliases)))
        (owner (tramp-async-request-write-buffer request))
        buffers)
    (when owner
      (unless (and (buffer-live-p owner)
                   (with-current-buffer owner
                     (or (member buffer-file-name paths)
                         (member buffer-file-truename paths))))
        (error "Saving buffer was killed or changed its visited file")))
    ;; Only inspect in-memory names.  `find-buffer-visiting' can do remote I/O.
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (or (member buffer-file-name paths)
                  (and buffer-file-truename (member buffer-file-truename paths)))
          (run-hooks 'tramp-async-before-buffer-write-hook))
        (when (and (not (and owner (or (eq buffer owner)
                                       (eq (buffer-base-buffer buffer) owner))))
                   (or (member buffer-file-name paths)
                       (and buffer-file-truename (member buffer-file-truename paths))))
          (when (buffer-modified-p)
            (error "File has unsaved Emacs edits: %s" buffer-file-name))
          (push buffer buffers))))
    (dolist (buffer buffers)
      (let ((guard (lambda (&rest _) (user-error "Background file write in progress"))))
        (with-current-buffer buffer (add-hook 'before-change-functions guard nil t))
        (push (cons buffer guard) (tramp-async-request-locks request))))))

(defun tramp-async--event (worker process request message)
  "Dispatch REQUEST's adapter event MESSAGE from WORKER and PROCESS.
Suspend the request timeout while its local handler reads user input.
The handler is supplied by trusted local Elisp, never by the child."
  (unless (tramp-async-request-rpc request)
    (let* ((token (plist-get message :token))
           (remaining (max 0.01 (- (tramp-async-request-deadline request) (float-time))))
           value failure)
      (setf (tramp-async-request-rpc request) token)
      (cancel-timer (tramp-async-request-timer request))
      (unwind-protect
          (condition-case err
              (if-let* ((handler (tramp-async-request-event-handler request)))
                  (setq value (funcall handler (plist-get message :payload)))
                (error "This request has no adapter event handler"))
            ((error quit) (setq failure err)))
        ;; User input can run timers, cancel the request or retire its worker.
        ;; Never send a late decision to another request or process generation.
        (when (and (eq process (tramp-async--worker-process worker))
                   (eq request (tramp-async--worker-active worker))
                   (not (tramp-async-request-done request)))
          (setf (tramp-async-request-rpc request) nil
                (tramp-async-request-deadline request) (+ (float-time) remaining)
                (tramp-async-request-timer request)
                (run-at-time remaining nil #'tramp-async--abort request 'timeout "Worker job timed out"))
          (condition-case err
              (async-send process :event 'adapter-reply :id (tramp-async-request-id request)
                          :token token :value value :error failure)
            (error (tramp-async--abort request 'error (error-message-string err)))))))))

(defun tramp-async--message (worker process message)
  "Handle a deferred MESSAGE from WORKER's captured PROCESS generation."
  (when (eq process (tramp-async--worker-process worker))
    (let ((request (tramp-async--worker-active worker)))
      (pcase (plist-get message :event)
        ('ready
         (setf (tramp-async--worker-ready worker) t)
         (tramp-async--send worker))
        ('check-write
         (when (and request (= (plist-get message :id) (tramp-async-request-id request)))
           (let ((error-data (condition-case err
                                 (progn (tramp-async--guard-write request message) nil)
                               (error err))))
             (condition-case err
                 (async-send process :event 'write-decision
                             :id (tramp-async-request-id request) :error error-data)
               (error (tramp-async--abort request 'error (error-message-string err)))))))
        ('adapter-event
         (when (and request (equal (plist-get message :id) (tramp-async-request-id request)))
           (tramp-async--event worker process request message)))
        ('result
         (when (and request (= (plist-get message :id) (tramp-async-request-id request)))
           (setf (tramp-async--worker-active worker) nil)
           (tramp-async--settle request (plist-get message :result))
           (tramp-async--pump worker)))))))

(defun tramp-async--exited (worker process)
  "Report unexpected exit of WORKER's PROCESS."
  (when (eq process (tramp-async--worker-process worker))
    (if-let* ((request (tramp-async--worker-active worker)))
        (tramp-async--abort request 'error "Worker exited; remote outcome may be unknown")
      (tramp-async--retire worker)
      (tramp-async--pump worker))))

(defun tramp-async--start (worker)
  "Start WORKER locally using emacs-async and await its ready message."
  (let ((default-directory temporary-file-directory)
        (async-quiet-switch "-Q") (async-child-init nil)
        (async-library (symbol-file 'async-start 'defun))
        (async-prompt-for-password nil) (async-debug nil)
        (async-send-over-pipe t) process)
    (setq process
          (async-start
           `(lambda ()
              (add-to-list 'load-path ,tramp-async--directory)
              (require 'tramp-async)
              (tramp-async-worker-main ',tramp-async-worker-settings))
           (lambda (message)
             (run-at-time 0 nil (lambda () (tramp-async--message worker process message))))))
    (setf (tramp-async--worker-process worker) process)
    (set-process-query-on-exit-flag process nil)
    ;; async's sentinel does not report signal deaths to the callback.
    ;; Persistent workers communicate through async-send, never final values.
    (set-process-sentinel
     process (lambda (proc _event)
               (when (memq (process-status proc) '(exit signal failed))
                 (run-at-time 0 nil #'tramp-async--exited worker proc))))
    ;; Bound async.el's accumulated protocol buffer after complete lines.
    (let ((filter (process-filter process)))
      (set-process-filter
       process (lambda (proc chunk)
                 (funcall filter proc chunk)
                 (when (buffer-live-p (process-buffer proc))
                   (with-current-buffer (process-buffer proc)
                     (when (and (boundp 'async-read-marker)
                                (markerp async-read-marker)
                                (> async-read-marker (point-min)))
                       (delete-region (point-min) async-read-marker)))))))))

(defun tramp-async--send (worker)
  "Send WORKER's active request only after the worker is ready to read."
  (when-let* ((request (tramp-async--worker-active worker)))
    (when (tramp-async--worker-ready worker)
      (condition-case err
          (async-send
           (tramp-async--worker-process worker)
           :id (tramp-async-request-id request)
           :directory (tramp-async-request-directory request)
           :function (tramp-async-request-function request)
           :arguments (tramp-async-request-arguments request)
           :library (tramp-async-request-library request)
           :events (and (tramp-async-request-event-handler request) t)
           :visiting (delq nil (mapcar (lambda (buffer)
                                        (buffer-local-value 'buffer-file-name buffer))
                                      (buffer-list))))
        (error (tramp-async--abort request 'error (error-message-string err)))))))

(defun tramp-async--expire (worker)
  "Release idle WORKER if it still has no jobs."
  (unless (or (tramp-async--worker-active worker) (tramp-async--worker-queue worker))
    (tramp-async--retire worker)
    (when (eq worker (gethash (tramp-async--worker-key worker) tramp-async--workers))
      (remhash (tramp-async--worker-key worker) tramp-async--workers))))

(defun tramp-async--pump (worker)
  "Dispatch the next FIFO request on WORKER."
  (when (and (eq worker (gethash (tramp-async--worker-key worker) tramp-async--workers))
             (not (tramp-async--worker-active worker)))
    (when (timerp (tramp-async--worker-idle worker))
      (cancel-timer (tramp-async--worker-idle worker)))
    (if-let* ((request (pop (tramp-async--worker-queue worker))))
        (if (<= (tramp-async-request-deadline request) (float-time))
            ;; Timers may be delayed by synchronous work in the parent.
            ;; Never execute an expired mutation, even before its timer runs.
            (tramp-async--abort request 'timeout "Worker job timed out while queued")
          (setf (tramp-async--worker-active worker) request)
          (condition-case err
              (if (tramp-async--worker-process worker)
                  (tramp-async--send worker)
                (tramp-async--start worker))
            (error (tramp-async--abort request 'error (error-message-string err)))))
      (setf (tramp-async--worker-idle worker)
            (run-at-time tramp-async-idle-timeout nil #'tramp-async--expire worker)))))

(cl-defun tramp-async-call (directory function arguments callback
                                     &key library (timeout tramp-async-timeout) (mutating t)
                                     write-buffer event-handler (lane 'files))
  "Run trusted FUNCTION with ARGUMENTS in DIRECTORY, returning a request handle.
CALLBACK receives a result plist asynchronously.  LIBRARY is an optional
absolute local library filename to load in the worker.  TIMEOUT includes
queueing and startup.  MUTATING controls unknown-outcome reporting.
LANE is files (the default) or commands.  Each connection serializes jobs
within a lane; the two lanes run independently.  Keep managed writes in
the files lane.  Commands can mutate files too; callers must order jobs
explicitly when a command and a file operation depend on each other.
WRITE-BUFFER identifies the originating buffer of a snapshot save.  Only
that buffer and its indirect buffers may be dirty and edited during the
write; the caller must reconcile its modification tick after completion.
EVENT-HANDLER optionally handles child adapter events, returning readable
reply data.  It runs outside the process filter and may ask for user input;
time spent in the handler is excluded from TIMEOUT.  It must not wait for
another job on the same worker.  Errors are returned to the child.
FUNCTION must be a named function; this API is for trusted Elisp callers,
not code or function names supplied by a remote model or untrusted input."
  (unless (and (stringp directory) (file-name-absolute-p directory)
               (symbolp function) (functionp callback) (listp arguments)
               (numberp timeout) (> timeout 0)
               (memq lane '(files commands))
               (or (null write-buffer) (buffer-live-p write-buffer))
               (or (null event-handler) (functionp event-handler))
               (or (null library)
                   (and (stringp library) (file-name-absolute-p library)
                        (not (file-remote-p library)))))
    (error "Invalid asynchronous request"))
  (let* ((key (cons (or (file-remote-p directory nil 'never) :local) lane))
         (worker (or (gethash key tramp-async--workers)
                     (puthash key (tramp-async--worker-create :key key) tramp-async--workers))))
    (when (>= (length (tramp-async--worker-queue worker)) tramp-async-queue-limit)
      (error "TRAMP asynchronous queue is full"))
    (let ((request (tramp-async--request-create
                    :id (cl-incf tramp-async--serial) :worker worker
                    :directory directory :function function :arguments arguments
                    :library library :callback callback :timeout timeout :mutating (and mutating t)
                    :write-buffer write-buffer :event-handler event-handler)))
      (setf (tramp-async-request-deadline request) (+ (float-time) timeout)
            (tramp-async-request-timer request)
            (run-at-time timeout nil #'tramp-async--abort request 'timeout "Worker job timed out")
            (tramp-async--worker-queue worker)
            (nconc (tramp-async--worker-queue worker) (list request)))
      (run-at-time 0 nil #'tramp-async--pump worker)
      request)))

(cl-defun tramp-async-submit (directory operation arguments callback
                                       &key (timeout tramp-async-timeout))
  "Run OPERATION with plist ARGUMENTS in DIRECTORY and call CALLBACK.
Operations: read-file, write-file, edit-file, directory-files,
file-attributes, file-truename, process-file.  See `tramp-async-worker-run'
for arguments and result values.  TIMEOUT includes queueing and startup.
Commands use an independent worker; all file operations share one FIFO."
  (unless (memq operation '(read-file write-file edit-file directory-files
                           file-attributes file-truename process-file))
    (error "Unknown asynchronous operation: %s" operation))
  (tramp-async-call directory 'tramp-async-worker-run (list operation arguments)
                    callback :timeout timeout
                    :lane (if (eq operation 'process-file) 'commands 'files)
                    :mutating (memq operation '(write-file edit-file process-file))))

(defun tramp-async-shutdown ()
  "Cancel all requests and release all worker processes."
  (interactive)
  (maphash
   (lambda (_key worker)
     (let ((active (tramp-async--worker-active worker)))
       (dolist (request (append (when active (list active)) (tramp-async--worker-queue worker)))
         (tramp-async--settle request
                              (list :status 'cancelled :error '(error "Worker shutdown")
                                    :uncertain (and (eq request active)
                                                    (tramp-async-request-mutating request)))))
       (setf (tramp-async--worker-active worker) nil (tramp-async--worker-queue worker) nil)
       (tramp-async--retire worker)))
   tramp-async--workers)
  (clrhash tramp-async--workers))

(defun tramp-async-unload-function ()
  "Release workers before unloading."
  (when (featurep 'tramp-async-files)
    (tramp-async-files-mode -1))
  (tramp-async-shutdown)
  nil)

(provide 'tramp-async)
;;; tramp-async.el ends here
