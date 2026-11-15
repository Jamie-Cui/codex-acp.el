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

;; The parent and child worker implementations share this file.

;;; Code:
;;; Core API


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
  (tramp-async-files-mode -1)
  (tramp-async-shutdown)
  nil)

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

;;; File operations in the worker


(require 'dired)

(define-error 'tramp-async-files-conflict "File changed since visited or saved" 'file-error)

(defconst tramp-async-files-worker-settings
  '(make-backup-files backup-inhibited backup-by-copying
    backup-by-copying-when-linked backup-by-copying-when-mismatch
    backup-directory-alist tramp-backup-directory-alist version-control
    kept-old-versions kept-new-versions delete-old-versions
    file-precious-flag file-preserve-symlinks-on-save)
  "Readable save settings accepted from the parent.")

(defun tramp-async-files-worker--stamp (attributes)
  "Extract change-sensitive fields from ATTRIBUTES, excluding access time."
  (when attributes
    (list (file-attribute-modification-time attributes)
          (file-attribute-status-change-time attributes)
          (file-attribute-size attributes)
          (file-attribute-file-identifier attributes))))

(defun tramp-async-files-worker--metadata (files)
  "Return observed metadata for FILES, preserving unknown versus false values."
  (mapcar
   (lambda (file)
     (let* ((attributes (file-attributes file 'integer))
            (true (file-truename file)))
       (cons file
             (list (cons 'file-attributes attributes)
                   (cons 'file-truename true)
                   (cons 'file-exists-p (file-exists-p file))
                   (cons 'file-readable-p (file-readable-p file))
                   (cons 'file-writable-p (file-writable-p file))
                   (cons 'file-regular-p (file-regular-p file))
                   (cons 'file-directory-p (file-directory-p file))
                   (cons 'file-symlink-p (file-symlink-p file))
                   (cons 'file-modes (file-modes file))
                   (cons 'file-name-case-insensitive-p (file-name-case-insensitive-p file))
                   (cons 'identity (when-let* ((attrs (file-attributes true 'integer)))
                                     (file-attribute-file-identifier attrs)))))))
   (delete-dups (delq nil files))))

(defun tramp-async-files-worker-context (file limit locals envrc)
  "Prepare FILE's LOCALS, optional ENVRC executable and metadata within LIMIT."
  (let* ((name (expand-file-name file))
         (path (file-truename name))
         (variables (and locals (tramp-async-files-worker-locals name limit))))
    (append (list :locals variables
                  :metadata (tramp-async-files-worker--metadata
                             (list name path (file-name-directory path) (car variables))))
            (when envrc
              (list :envrc (tramp-async-files-worker-envrc
                            (if (file-directory-p path) path (file-name-directory path)) envrc))))))

(cl-defun tramp-async-files-worker-visit (file limit &optional coding locals switches envrc must-exist)
  "Read all of FILE up to LIMIT bytes, using optional CODING.
Return text, coding, attributes and a revision for subsequent saves.
Missing files return an empty new-file snapshot.  LOCALS requests directory
variables as data.  SWITCHES allows directory visits using Dired listings.
MUST-EXIST preserves Dired's rejection of removed entries and dangling links."
  (let* ((path (tramp-async-worker-path file))
         (attributes (file-attributes path 'integer)))
    (when (and must-exist (null attributes))
      (error "File no longer exists or symlink target is missing: %s" file))
    (when (and switches (eq (car attributes) t))
      (cl-return-from tramp-async-files-worker-visit
        (tramp-async-files-worker-dired file switches limit locals nil nil envrc)))
    (when (and attributes (not (file-regular-p path)))
      (error "Not a regular file; use Dired for directories: %s" file))
    (with-temp-buffer
      (let ((coding-system-for-read coding))
        (when attributes (tramp-async-worker--read path limit)))
      (let ((after (file-attributes path 'integer)))
        (unless (equal (tramp-async-files-worker--stamp attributes)
                       (tramp-async-files-worker--stamp after))
          (error "File changed while being read; open it again"))
        (append
         (tramp-async-files-worker-context file limit locals envrc)
         (list :path path :name (expand-file-name file) :text (buffer-string)
              :coding (if attributes buffer-file-coding-system (or coding 'utf-8-unix))
              :attributes after :parent-exists (file-directory-p (file-name-directory path))
              :writable (or (file-writable-p path)
                            (and (not after) (not (file-exists-p (file-name-directory path)))))
              :version (and after
                            (list path (tramp-async-files-worker--stamp after)
                                  (secure-hash 'sha256 (current-buffer))))))))))

(cl-defun tramp-async-files-worker-save (file text coding expected limit settings arg backed-up
                                            &optional create-parents (modified t))
  "Save TEXT to FILE using CODING after comparing EXPECTED revision.
LIMIT bounds content size.  SETTINGS, ARG and BACKED-UP preserve backup
preferences.  EXPECTED is a version from visiting, or (:mtime TIME) for a
buffer originally visited through synchronous Emacs APIs.  CREATE-PARENTS
authorizes creating missing parent directories.  MODIFIED nil makes an
existing unmodified file a no-op, but still recreates a deleted file."
  (unless modified
    (if (file-exists-p file)
        (cl-return-from tramp-async-files-worker-save '(:unchanged t))
      (setq expected nil)))
  (let* ((current (tramp-async-files-worker-visit file limit))
         (version (plist-get current :version))
         (attributes (plist-get current :attributes))
         (path (plist-get current :path)))
    (unless (if (eq (car-safe expected) :mtime)
                (if attributes
                    (equal (cadr expected) (file-attribute-modification-time attributes))
                  (equal (cadr expected) 0))
              (equal expected version))
      (signal 'tramp-async-files-conflict (list file version)))
    (when (> (string-bytes (encode-coding-string text coding)) limit)
      (error "File exceeds the asynchronous visit/save size limit"))
    (tramp-async-worker--check-write path)
    ;; Never accept arbitrary variable bindings from serialized requests.
    (unless (cl-every (lambda (entry) (memq (car entry) tramp-async-files-worker-settings)) settings)
      (error "Unsupported save setting"))
    (when create-parents
      (setq tramp-async-worker--mutating t)
      (make-directory (file-name-directory path) t))
    (cl-progv (mapcar #'car settings) (mapcar #'cdr settings)
      (with-temp-buffer
        (insert text)
        (setq buffer-file-name path buffer-file-truename path
              buffer-file-coding-system coding buffer-backed-up backed-up)
        (set-visited-file-modtime (if attributes (file-attribute-modification-time attributes) 0))
        (let ((create-lockfiles nil) (vc-handled-backends nil)
              ;; async.el binds UTF-8 for its protocol; do not inherit it for
              ;; the actual visited file's encoding or newline conversion.
              (coding-system-for-write coding)
              (before-save-hook nil) (after-save-hook nil)
              (write-contents-functions nil) (write-file-functions nil)
              (require-final-newline nil))
          (setq tramp-async-worker--mutating t)
          (save-buffer arg))
        (let ((result (tramp-async-files-worker-visit file limit)))
          ;; Do not copy saved text back through IPC a second time.
          (setq result (plist-put result :text nil))
          (cl-remf result :locals)
          (plist-put result :backed-up buffer-backed-up))))))

(defun tramp-async-files-worker-completions (directory)
  "Return file name completions in DIRECTORY, including directory slashes."
  (let* ((path (file-name-as-directory (tramp-async-worker-path directory)))
         (names (file-name-all-completions "" path)))
    (when (> (length names) 10000) (error "Too many directory entries to complete"))
    names))

(defun tramp-async-files-worker-locals (file limit)
  "Read FILE's directory-local variables as data, bounded by LIMIT.
Use the native reader and merge order, including .dir-locals-2.el.
Never apply the variables or evaluate their forms in the worker."
  (let ((dir-locals-directory-cache nil) (dir-locals-class-alist nil))
    (when-let* ((root (dir-locals-find-file file)))
      ;; Private Emacs 30 reader helpers are isolated here; using the native
      ;; reader retains nested directory and duplicate eval merge semantics.
      (let ((files (dir-locals--all-files root)) (size 0))
        (dolist (path files)
          (cl-incf size (file-attribute-size (file-attributes path)))
          (when (> size limit) (error "Directory-local variables exceed size limit")))
        (let ((class (dir-locals-read-from-dir root)))
          (cons root (dir-locals-get-class-variables class)))))))

(defun tramp-async-files-worker--directory-context (directories)
  "Resolve spelling aliases and case rules for DIRECTORIES in the worker.
Expansion keeps symlink spellings; it does not substitute their referents."
  (let (aliases case-fold)
    (dolist (dir (delete-dups (delq nil directories)))
      (let* ((name (file-name-as-directory (expand-file-name dir)))
             (home (concat (or (file-remote-p name nil 'never) "") "~/")))
        (dolist (alias (list dir (abbreviate-file-name name)))
          (setf (alist-get (file-name-as-directory alias) aliases nil nil #'equal) name))
        (setf (alist-get home aliases nil nil #'equal)
              (file-name-as-directory (expand-file-name home)))
        (setf (alist-get name case-fold nil nil #'equal) (file-name-case-insensitive-p name))))
    (list :aliases aliases :case-fold case-fold)))

(defun tramp-async-files-worker-dired (directory switches limit locals &optional subdirs subtrees envrc)
  "Return DIRECTORY's native Dired listing using SWITCHES, bounded by LIMIT.
LOCALS requests directory variables.  SUBDIRS are additional inserted dirs.
SUBTREES names expanded dired-subtree directories whose listings are needed."
  (let* ((name (file-name-as-directory (expand-file-name directory)))
         (path (file-name-as-directory (tramp-async-worker-path name)))
         (attributes (file-attributes path)))
    (unless (eq (car attributes) t) (error "Not a directory: %s" directory))
    (with-temp-buffer
      (setq default-directory name)
      (let ((enable-local-variables nil) (enable-dir-local-variables nil)
            (dired-mode-hook nil) (dired-before-readin-hook nil)
            (dired-after-readin-hook nil) (dired-auto-revert-buffer nil))
        (dired-mode name switches)
        (dired-readin)
        (dolist (subdir subdirs) (dired-insert-subdir (expand-file-name subdir name)))
        (when (> (buffer-size) limit) (error "Directory listing exceeds size limit"))
        (append
         (tramp-async-files-worker-context name limit locals envrc)
         (tramp-async-files-worker--directory-context
          (append (list directory name path) (mapcar #'car dired-subdir-alist) subdirs subtrees))
         (list :directory t :name name :path path :attributes attributes
              :text (buffer-substring-no-properties (point-min) (point-max))
              :switches dired-actual-switches
              :subtrees (mapcar (lambda (dir)
                                  (cons (directory-file-name (expand-file-name dir name))
                                        (tramp-async-files-worker-listing dir switches limit)))
                                subtrees)))))))

(defun tramp-async-files-worker-subtree (directory switches limit)
  "Return DIRECTORY's listing and resolved path context for a subtree."
  (append (tramp-async-files-worker--directory-context (list directory))
          (list :text (tramp-async-files-worker-listing directory switches limit))))

(defun tramp-async-files-worker-listing (directory switches limit)
  "Return raw insert-directory output for DIRECTORY, SWITCHES and LIMIT."
  (unless (file-directory-p directory) (error "Not a directory: %s" directory))
  (with-temp-buffer
    (insert-directory (file-name-as-directory directory) switches nil t)
    (when (> (buffer-size) limit) (error "Directory listing exceeds size limit"))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun tramp-async-files-worker--direnv-json (executable &rest arguments)
  "Run direnv EXECUTABLE with literal ARGUMENTS and return its JSON object.
Keep stderr and environment values out of diagnostic messages."
  (require 'json)
  (with-temp-buffer
    (let ((status (apply #'process-file executable nil '(t nil) nil arguments)))
      (unless (eq status 0) (error "Direnv %s failed (exit %s)" (car arguments) status)))
    (when (> (buffer-size) (* 1024 1024)) (error "Direnv output exceeds size limit"))
    (goto-char (point-min))
    (if (eobp) (make-hash-table :test #'equal)
      (condition-case nil
          (json-parse-buffer :object-type 'hash-table :null-object nil :false-object :false)
        (json-error (error "Invalid JSON from direnv"))))))

(defun tramp-async-files-worker-envrc (directory executable)
  "Discover DIRECTORY's direnv environment and export it in the worker.
Honor direnv's trust decision; never run allow or deny automatically."
  (let ((root (locate-dominating-file
               directory (lambda (dir)
                           (or (file-exists-p (expand-file-name ".envrc" dir))
                               (file-exists-p (expand-file-name ".env" dir)))))))
    (if (not root) '(:directory nil :result none)
      (let* ((default-directory (expand-file-name root))
             (status (tramp-async-files-worker--direnv-json executable "status" "--json"))
             (state (and (hash-table-p status) (gethash "state" status)))
             (found (and (hash-table-p state) (gethash "foundRC" state)))
             (allowed (and (hash-table-p found) (gethash "allowed" found)))
             (result
              (pcase allowed
                ((or 1 2) 'denied)
                ('nil 'none)
                (0
                 ;; Evaluation can have effects; cancellation must not replay it.
                 (setq tramp-async-worker--mutating t)
                 (let ((data (tramp-async-files-worker--direnv-json executable "export" "json")) pairs)
                   (unless (hash-table-p data) (error "Invalid direnv environment object"))
                   (maphash (lambda (key value)
                              (unless (and (stringp key) (not (string-empty-p key))
                                           (not (string-match-p "[=\0]" key))
                                           (or (null value) (and (stringp value) (not (string-search "\0" value)))))
                                (error "Invalid direnv environment entry"))
                              (push (cons key value) pairs)) data)
                   pairs))
                (_ (error "Unknown direnv permission status")))))
        (list :directory default-directory :result result)))))

;;; Dired operations in the worker

(require 'dired-aux)

(defconst tramp-async-dired-ops-worker-settings
  '(dired-recursive-copies dired-recursive-deletes dired-copy-preserve-time
                           dired-copy-dereference copy-directory-create-symlink dired-create-destination-dirs
                           dired-create-destination-dirs-on-trailing-dirsep dired-backup-overwrite
                           dired-vc-rename-file delete-by-moving-to-trash trash-directory
                           backup-directory-alist tramp-backup-directory-alist version-control kept-new-versions kept-old-versions
                           delete-old-versions)
  "Data-only settings accepted from the interactive Dired buffer.")

(defun tramp-async-dired-ops-worker--stamp (file)
  "Return FILE's identity and modification metadata without access time."
  (when-let* ((attrs (file-attributes file 'integer)))
    (list (car attrs) (nth 5 attrs) (nth 6 attrs) (nth 7 attrs)
          (nth 8 attrs) (nth 10 attrs) (nth 11 attrs))))

(defun tramp-async-dired-ops-worker--describe (file &optional tree)
  "Describe FILE and optional descendant TREE for in-memory buffer guards.
Resolve its parent but never replace a final symlink with its referent."
  (when file
    (let* ((file (directory-file-name (expand-file-name file)))
           (attrs (file-attributes file))
           (entry (expand-file-name (file-name-nondirectory file)
                                    (file-truename (file-name-directory file))))
           (tree (or tree (eq t (car attrs))))
           aliases)
      (dolist (name (plist-get tramp-async-worker--request :visiting))
        (when (equal (file-remote-p name) (file-remote-p file))
          (let ((true (file-truename name)))
            (when (or (equal true entry)
                      (and tree (string-prefix-p (file-name-as-directory entry) true)))
              (push (cons name (substring true (length entry))) aliases)))))
      (list :name file :entry entry :link (stringp (car attrs)) :tree tree
            :referent (file-truename file) :aliases aliases))))

(defvar tramp-async-dired-ops-worker--watch nil)

(defun tramp-async-dired-ops-worker--prompt (kind &rest args)
  "Relay an explicitly supported prompt KIND with ARGS to the UI."
  (let* ((before (mapcar #'tramp-async-dired-ops-worker--stamp tramp-async-dired-ops-worker--watch))
         (answer (tramp-async-worker-request (list :event 'query :kind kind :args args))))
    (unless (equal before (mapcar #'tramp-async-dired-ops-worker--stamp tramp-async-dired-ops-worker--watch))
      (signal 'file-error '("File changed while awaiting confirmation")))
    answer))

(defun tramp-async-dired-ops-worker--item (operation from to thunk stamps)
  "Run one OPERATION FROM TO using THUNK after checking confirmation STAMPS.
Report each item before advancing, including failures and skipped overwrites."
  (let* ((source (tramp-async-dired-ops-worker--describe from))
         (target (tramp-async-dired-ops-worker--describe to (plist-get source :tree)))
         (before (mapcar #'tramp-async-dired-ops-worker--stamp (delq nil (list from to))))
         (result (list :operation operation :from from :to to :source source :target target))
         failure started)
    (condition-case err
        (progn
          (when-let* ((confirmed (and to (assoc to stamps))))
            (unless (equal (cdr confirmed) (tramp-async-dired-ops-worker--stamp to))
              (signal 'file-error (list "Destination changed while awaiting confirmation" to))))
          (tramp-async-worker-request (list :event 'guard :item result))
          (unless (equal before (mapcar #'tramp-async-dired-ops-worker--stamp (delq nil (list from to))))
            (signal 'file-error (list "File changed while awaiting buffer protection" from to)))
          ;; Conservative: a recursive operation/backup may mutate before an
          ;; error or another question.  Never automatically replay this item.
          (setq tramp-async-worker--mutating t started t)
          (let ((tramp-async-dired-ops-worker--watch (delq nil (list from to)))
                (old-failures dired-create-files-failures))
            (funcall thunk)
            ;; Native copying can catch file-date-error after writing data.
            ;; Its failure list still makes this an incomplete operation.
            (unless (eq old-failures dired-create-files-failures)
              (signal 'file-error (list "Copied data but could not preserve file metadata" from to))))
          (setq result (plist-put result :status 'ok)))
      (file-already-exists
       (setq failure err result (append result (list :status 'skipped :error err))))
      (error
       (setq failure err result (append result (list :status 'error :error err :uncertain started)))))
    (tramp-async-worker-request (list :event 'done :item result))
    ;; Native Dired continues on file-error, but stops on quit.  Report the
    ;; error above before re-signalling into its own accounting boundary.
    (when failure (signal 'file-error (list (error-message-string failure))))
    result))

(defun tramp-async-dired-ops-worker-run (operation files target settings arg)
  "Perform OPERATION on FILES with TARGET, SETTINGS and native prefix ARG.
Only copy, rename and delete are accepted.  Caller already confirmed the
initial delete selection.  Native per-directory/overwrite questions remain."
  (unless (memq operation '(copy rename delete)) (error "Unknown Dired operation"))
  (unless (and files (cl-every (lambda (file) (and (stringp file) (file-name-absolute-p file))) files))
    (error "Expected absolute Dired filenames"))
  (dolist (file files)
    (when (member (file-name-nondirectory (directory-file-name file)) '("." ".." ""))
      (error "Refusing a dot entry or filesystem root")))
  (dolist (setting settings)
    (unless (memq (car setting) tramp-async-dired-ops-worker-settings)
      (error "Unknown Dired setting: %s" (car setting))))
  (cl-progv (mapcar #'car settings) (mapcar #'cdr settings)
            (let ((dired-recursive-copies dired-recursive-copies)
                  (dired-recursive-deletes dired-recursive-deletes)
                  (dired-copy-dereference (if (and (eq operation 'copy) (equal arg '(4)))
                                              (not dired-copy-dereference) dired-copy-dereference))
                  (dired-do-revert-buffer nil) (dired-copy-how-to-fn nil)
                  (copy (symbol-function 'dired-copy-file))
                  (rename (symbol-function 'dired-rename-file))
                  (create (symbol-function 'dired-create-files))
                  (query (symbol-function 'dired-query))
                  (mkdir (symbol-function 'make-directory))
                  (destinations (make-hash-table :test #'equal))
                  stamps completed failure)
              (with-temp-buffer
                ;; This is a UI-less Dired context, never a directory visit.
                (setq major-mode 'dired-mode dired-directory default-directory
                      dired-subdir-alist (list (cons default-directory (point-marker))))
                (cl-letf
                 (((symbol-function 'make-directory)
                   (lambda (&rest args)
                     (setq tramp-async-worker--mutating t)
                     (apply mkdir args)))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest args) (apply #'tramp-async-dired-ops-worker--prompt 'yes-or-no-p args)))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest args) (apply #'tramp-async-dired-ops-worker--prompt 'y-or-n-p args)))
                  ((symbol-function 'read-char-choice)
                   (lambda (&rest args) (apply #'tramp-async-dired-ops-worker--prompt 'read-char-choice args)))
                  ((symbol-function 'read-answer)
                   (lambda (&rest args) (apply #'tramp-async-dired-ops-worker--prompt 'read-answer args)))
                  ((symbol-function 'dired-get-marked-files) (lambda (&rest _) files))
                  ((symbol-function 'dired-mark-read-file-name) (lambda (&rest _) target))
                  ((symbol-function 'dired-dwim-target-directory) (lambda () default-directory))
                  ((symbol-function 'dired-dwim-target-defaults) (lambda (&rest _) nil))
                  ((symbol-function 'dired-goto-file) #'ignore)
                  ((symbol-function 'dired-file-marker) #'ignore)
                  ((symbol-function 'dired-move-to-filename) #'ignore)
                  ((symbol-function 'dired-add-file) #'ignore)
                  ((symbol-function 'dired-remove-file) #'ignore)
                  ((symbol-function 'dired-relist-entry) #'ignore)
                  ((symbol-function 'dired-post-do-command) #'ignore)
                  ((symbol-function 'dired-log) #'ignore)
                  ((symbol-function 'dired-log-summary) #'ignore)
                  ((symbol-function 'dired-query)
                   (lambda (symbol format &rest args)
                     (let* ((file (car args))
                            (stamp (and (eq symbol 'overwrite-query)
                                        (tramp-async-dired-ops-worker--stamp file)))
                            (answer (apply query symbol format args)))
                       (when (and answer (eq symbol 'overwrite-query))
                         (push (cons file stamp) stamps))
                       answer)))
                  ((symbol-function 'dired-create-files)
                   (lambda (creator label names constructor &optional marker)
                     (funcall create creator label names
                              (lambda (from)
                                (let ((to (funcall constructor from)))
                                  (puthash from to destinations) to)) marker)))
                  ((symbol-function 'dired-copy-file)
                   (lambda (from to overwrite)
                     (push from completed)
                     (tramp-async-dired-ops-worker--item
                      'copy from (gethash from destinations)
                      (lambda () (funcall copy from to overwrite)) stamps)))
                  ((symbol-function 'dired-rename-file)
                   (lambda (from to overwrite)
                     (push from completed)
                     (tramp-async-dired-ops-worker--item
                      'rename from to (lambda () (funcall rename from to overwrite)) stamps))))
                 ;; This boundary returns partial progress, never disguises a failed
                 ;; or interrupted recursive operation as a successful whole batch.
                 (condition-case err
                     (pcase operation
                       ('copy (dired-do-create-files 'copy 'dired-copy-file "Copy" nil))
                       ('rename (dired-do-create-files 'move 'dired-rename-file "Move" nil nil "Rename"))
                       ('delete
                        (dolist (file files)
                          (push file completed)
                          (condition-case nil
                              (tramp-async-dired-ops-worker--item
                               'delete file nil (lambda () (dired-delete-file file dired-recursive-deletes t)) nil)
                            (file-error nil)))))
                   ((error quit) (setq failure err)))
                 (unless failure
                   (dolist (file files)
                     (unless (member file completed)
                       (tramp-async-worker-request
                        (list :event 'done :item (list :operation operation :from file
                                                       :status 'skipped :error '(error "Same destination; skipped")))))))
                 (list :error failure :stopped (and failure t)))))))

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

(provide 'tramp-async-worker)
(provide 'tramp-async-files-worker)
(provide 'tramp-async-dired-ops-worker)
(provide 'tramp-async-files)
(provide 'tramp-async-dired-ops)
(provide 'tramp-async-dired)
(provide 'tramp-async-batch)
(provide 'tramp-async)
;;; tramp-async.el ends here
