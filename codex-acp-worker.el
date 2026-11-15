;;; codex-acp-worker.el --- Codex policy inside a tramp-async worker -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jamie Cui
;; Author: Jamie Cui <jamie.cui@outlook.com>
;; Assisted-by: Codex:GPT-6
;; Maintainer: Jamie Cui <jamie.cui@outlook.com>
;; Version: 0.1.0
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
;; Run by a local batch Emacs subprocess.  All filesystem and command calls,
;; including TRAMP path resolution, stay outside the interactive Emacs process.
;; Loaded by tramp-async for trusted ACP jobs; no separate transport is used.

;;; Code:

(require 'cl-lib)
(require 'tramp-async)
(require 'map)
(require 'subr-x)
(require 'tramp)

(defun codex-acp-worker--required-string (args key)
  "Return string KEY from ARGS or signal an error."
  (let ((value (map-elt args key)))
    (unless (stringp value) (error "Expected string field %s" key))
    value))

(define-error 'codex-acp-directory-required "Directory access requires approval" 'file-error)

(defun codex-acp-worker--path (request path &optional skill-read directory)
  "Resolve PATH within REQUEST's workspace, allowing SKILL-READ roots.
DIRECTORY means PATH names a directory even if it does not exist yet.
On a root violation, report the canonical directory before file contents
are accessed or a command runs."
  (unless (and (stringp path) (not (string-empty-p path)))
    (error "Expected a nonempty file path"))
  (let ((local (if (file-remote-p path)
                   (tramp-file-name-localname (tramp-dissect-file-name path)) path)))
    (when (string-prefix-p "~" local)
      (error "Use relative or absolute paths, not home expansion")))
  (condition-case err
      (tramp-async-worker-path
       path (unless (equal (map-elt request 'scope) "host")
              (append (list default-directory)
                      (map-elt request 'allowedDirectories)
                      (when skill-read (map-elt request 'skills)))))
    (tramp-async-path-outside-roots
     (let ((true (cadr err)))
       (signal 'codex-acp-directory-required
               (list (file-name-as-directory
                      (if (or directory (file-directory-p true)) true
                        (file-name-directory true)))))))))

(defun codex-acp-worker--file-tool (request)
  "Execute REQUEST's file tool and return its text result."
  (let* ((tool (codex-acp-worker--required-string request 'tool))
         (args (map-elt request 'arguments))
         (path (codex-acp-worker--path
                request (codex-acp-worker--required-string args 'path)
                (member tool '("tramp_read_file" "tramp_list_directory"
                               "tramp_file_truename" "tramp_file_attributes"))
                (equal tool "tramp_list_directory")))
         (common (list :path path :max-bytes (map-elt request 'fileSizeLimit)
                       :max-chars (map-elt request 'outputLimit))))
    (pcase tool
      ("tramp_file_truename" (file-local-name path))
      ("tramp_file_attributes"
       (prin1-to-string (tramp-async-worker-run 'file-attributes common)))
      ("tramp_list_directory"
       (string-join (tramp-async-worker-run 'directory-files common) "\n"))
      ("tramp_read_file"
       (tramp-async-worker-run
        'read-file (append common (list :line (or (map-elt args 'line) 1)
                                      :lines (or (map-elt args 'limit) 200)))))
      ((or "tramp_write_file" "tramp_edit_file")
       (tramp-async-worker-run
        (if (equal tool "tramp_write_file") 'write-file 'edit-file)
        (append common
                (if (equal tool "tramp_write_file")
                    (list :text (codex-acp-worker--required-string args 'text))
                  (list :old (codex-acp-worker--required-string args 'old)
                        :new (codex-acp-worker--required-string args 'new)))))
       (format "Wrote %s. Existing Emacs buffers may need reverting." (file-local-name path)))
      (_ (error "Unknown file tool: %s" tool)))))

(defun codex-acp-worker--run-command (request)
  "Run REQUEST's literal argv and return (SUCCESS . TEXT)."
  (let* ((args (map-elt request 'arguments))
         (result (tramp-async-worker-run
                  'process-file
                  (list :argv (map-elt args 'argv)
                        :cwd (codex-acp-worker--path request (or (map-elt args 'cwd) ".") nil t)
                        :max-chars (map-elt request 'outputLimit)
                        :command-timeout (map-elt request 'commandTimeout)))))
    (cons (and (eq (plist-get result :status) 'exit) (zerop (plist-get result :exit)))
          (concat (plist-get result :output)
                  (when (plist-get result :truncated) "\n[Output truncated]")
                  (format "\nExit status: %s" (plist-get result :exit))))))

(defun codex-acp-worker-execute (request)
  "Execute a trusted adapter REQUEST and return (SUCCESS . TEXT)."
  (let* ((result (if (equal (map-elt request 'tool) "tramp_run_command")
                     (codex-acp-worker--run-command request)
                   (cons t (codex-acp-worker--file-tool request))))
         (text (cdr result)) (limit (map-elt request 'outputLimit)))
    (cons (car result) (substring text 0 (min limit (length text))))))

(defun codex-acp-worker--skill-metadata (path source)
  "Read completion metadata from remote SKILL.md at PATH and SOURCE."
  (with-temp-buffer
    (insert-file-contents path nil 0 8192)
    (let ((name (file-name-nondirectory (directory-file-name (file-name-directory path))))
          description)
      (goto-char (point-min))
      (when (looking-at "---[ \t]*$")
        (let ((end (save-excursion (forward-line) (re-search-forward "^---[ \t]*$" nil t))))
          (when end
            (when (re-search-forward "^name:[ \t]*\\(.+\\)$" end t)
              (setq name (string-trim (match-string 1) "[ \t\"']+" "[ \t\"']+")))
            (goto-char (point-min))
            (when (re-search-forward "^description:[ \t]*\\(.+\\)$" end t)
              (setq description
                    (string-trim (match-string 1) "[ \t\"']+" "[ \t\"']+"))))))
      (when (string-match-p "\\`[[:alnum:]_][[:alnum:]_:.+-]*\\'" name)
        `((name . ,name) (description . ,(or description ""))
          (path . ,path) (source . ,source))))))

(defun codex-acp-worker-skills (roots)
  "Discover direct skill children of ROOTS; return skills and warnings."
  (let (skills warnings)
    (dolist (root roots)
      (condition-case err
          (when (file-directory-p (cdr root))
            (dolist (directory (tramp-async-worker-run
                                'directory-files (list :path (cdr root) :full t :max-entries 1000)))
              (let ((path (expand-file-name "SKILL.md" directory)))
                (when (and (file-directory-p directory) (file-regular-p path))
                  (when-let* ((skill (codex-acp-worker--skill-metadata path (car root))))
                    (push skill skills))))))
        (file-error (push (error-message-string err) warnings))))
    (list :skills (nreverse skills) :warnings (nreverse warnings))))

(defun codex-acp-worker--skill-content (skill)
  "Read selected remote SKILL text for SKILL, with a bounded size."
  (let* ((path (map-elt skill 'path))
         (attributes (file-attributes path)))
    (unless (and attributes (<= (file-attribute-size attributes) 65536))
      (error "Remote skill is missing or exceeds 64 KiB: %s" (map-elt skill 'name)))
    (with-temp-buffer
      (tramp-async-worker--read (file-truename path) 65536)
      (format (concat "The user selected the remote skill %s. Follow its instructions "
                      "for this turn. Its supporting files are on the session host "
                      "under %s; read them with the Emacs workspace tools when needed.\n\n%s")
              (map-elt skill 'name) (file-local-name (file-name-directory path))
              (buffer-string)))))

(defun codex-acp-worker-prompt-input (input)
  "Expand remote skill placeholders in trusted INPUT into text."
  (mapcar (lambda (item)
            (if (equal (map-elt item 'type) "remoteSkill")
                `((type . "text") (text . ,(codex-acp-worker--skill-content (map-elt item 'skill))))
              item)) input))

(provide 'codex-acp-worker)
;;; codex-acp-worker.el ends here
