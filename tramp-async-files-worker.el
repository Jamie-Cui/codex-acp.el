;;; tramp-async-files-worker.el --- Optional file and Dired snapshot jobs -*- lexical-binding: t -*-

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
;; Optional file and Dired snapshot jobs.
;;; Code:
(require 'tramp-async-worker)

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


(provide 'tramp-async-files-worker)
(provide 'tramp-async-dired-ops-worker)
;;; tramp-async-files-worker.el ends here
