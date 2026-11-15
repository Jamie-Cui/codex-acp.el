;;; codex-acp-agent-shell.el --- Agent Shell frontend for local Codex -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Optional integration with agent-shell.  Enable
;; `codex-acp-agent-shell-mode' to register the Codex Emacs agent, or call
;; `codex-acp-agent-shell-start'.  Loading this library starts no processes
;; and installs no advice.  Agent Shell is an optional runtime dependency.
;;
;; The compatibility block below isolates Agent Shell's private startup API:
;; current releases probe client factories repeatedly and require a command
;; even for an already running in-process client.  The adapter retains its
;; nil command so acp.el cannot accidentally restart a raw Codex process.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'codex-acp)

(defvar agent-shell-agent-configs)
(defvar agent-shell-path-resolver-function)

(declare-function agent-shell-make-agent-config "agent-shell" (&rest arguments))
(declare-function agent-shell-start "agent-shell" (&rest arguments))

(defvar-local codex-acp-agent-shell--client nil
  "Client owned by this Agent Shell buffer.")

(defvar codex-acp-agent-shell--starting nil
  "Non-nil while starting this integration through Agent Shell.")

(defvar codex-acp-agent-shell--starting-clients nil
  "Clients created during the current startup, for failure cleanup.")

(defvar codex-acp-agent-shell--config-source nil
  "Original callable Agent Shell configuration source.")

(defun codex-acp-agent-shell--live-p (client)
  "Return non-nil when CLIENT has a live process."
  (let ((process (map-elt client :process)))
    (and (processp process) (process-live-p process))))

(defun codex-acp-agent-shell--shutdown ()
  "Release this buffer's retained client."
  (when codex-acp-agent-shell--client
    (let ((client codex-acp-agent-shell--client))
      (setq codex-acp-agent-shell--client nil)
      (acp-shutdown :client client))))

(defun codex-acp-agent-shell--make-client (buffer)
  "Return BUFFER's retained client, creating it when needed.
Repeated startup probes reuse the same process.  A shut-down client is
replaced on restart.  Preserve complete TRAMP names in ACP requests."
  (with-current-buffer buffer
    (setq-local agent-shell-path-resolver-function #'identity)
    (unless (codex-acp-agent-shell--live-p codex-acp-agent-shell--client)
      (codex-acp-agent-shell--shutdown)
      (setq codex-acp-agent-shell--client (codex-acp-make-client buffer))
      (add-hook 'kill-buffer-hook #'codex-acp-agent-shell--shutdown nil t)
      (when codex-acp-agent-shell--starting
        (push codex-acp-agent-shell--client
              codex-acp-agent-shell--starting-clients)))
    codex-acp-agent-shell--client))

;;;###autoload
(defun codex-acp-agent-shell-make-config ()
  "Return an Agent Shell configuration for the Emacs Codex adapter.
Enable `codex-acp-agent-shell-mode' before starting this configuration."
  (require 'agent-shell)
  (agent-shell-make-agent-config
   :identifier 'codex-emacs
   :mode-line-name "Codex Emacs"
   :buffer-name "Codex Emacs"
   :shell-prompt "Codex> "
   :shell-prompt-regexp "Codex> "
   :needs-authentication nil
   :client-maker #'codex-acp-agent-shell--make-client
   :install-instructions "Install Codex CLI 0.162.1 or newer and run codex login locally."))

(defun codex-acp-agent-shell--configs ()
  "Add the Emacs adapter to the original callable configuration source."
  (cons #'codex-acp-agent-shell-make-config
        (remq #'codex-acp-agent-shell-make-config
              (funcall codex-acp-agent-shell--config-source))))

;;;; Compatibility with Agent Shell's external-process startup checks

(defun codex-acp-agent-shell--start (function &rest arguments)
  "Call Agent Shell startup FUNCTION with ARGUMENTS for this adapter.
Only bypass the nil-command executable check for this buffer's live
client.  Other agents and executable lookups keep their original behavior.
Release newly created clients if startup exits with an error or quit."
  (if (not (eq (map-elt (plist-get arguments :config) :identifier)
               'codex-emacs))
      (apply function arguments)
    (let ((codex-acp-agent-shell--starting t)
          (codex-acp-agent-shell--starting-clients nil)
          (find-executable (symbol-function 'executable-find))
          completed)
      (unwind-protect
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (command &optional remote)
                       (if (and (null command)
                                (codex-acp-agent-shell--live-p
                                 codex-acp-agent-shell--client))
                           codex-acp-program
                         (funcall find-executable command remote)))))
            (prog1 (apply function arguments)
              (setq completed t)))
        (unless completed
          (dolist (client codex-acp-agent-shell--starting-clients)
            ;; Cleanup must not replace the startup error or quit.
            (condition-case err
                (acp-shutdown :client client)
              (error (display-warning 'codex-acp
                                      (error-message-string err))))))))))

;;;###autoload
(define-minor-mode codex-acp-agent-shell-mode
  "Register the Codex Emacs agent and enable startup compatibility.
This global mode does not change the preferred agent or start Codex.
Disabling it removes registration and advice; running clients remain owned
by their conversation buffers until Agent Shell or buffer cleanup closes them."
  :global t
  :group 'codex-acp
  (if codex-acp-agent-shell-mode
      (progn
        (unless (require 'agent-shell nil t)
          (setq codex-acp-agent-shell-mode nil)
          (user-error "Install agent-shell before enabling this integration"))
        (unless (fboundp 'agent-shell--start)
          (setq codex-acp-agent-shell-mode nil)
          (user-error "This agent-shell version has no supported startup entry"))
        (if (functionp agent-shell-agent-configs)
            (unless (eq agent-shell-agent-configs
                        #'codex-acp-agent-shell--configs)
              (setq codex-acp-agent-shell--config-source
                    agent-shell-agent-configs
                    agent-shell-agent-configs
                    #'codex-acp-agent-shell--configs))
          (add-to-list 'agent-shell-agent-configs
                       #'codex-acp-agent-shell-make-config))
        (advice-add 'agent-shell--start :around
                    #'codex-acp-agent-shell--start))
    (if (and (boundp 'agent-shell-agent-configs)
             (eq agent-shell-agent-configs #'codex-acp-agent-shell--configs))
        (setq agent-shell-agent-configs codex-acp-agent-shell--config-source)
      (when (and (boundp 'agent-shell-agent-configs)
                 (listp agent-shell-agent-configs))
        (setq agent-shell-agent-configs
              (remq #'codex-acp-agent-shell-make-config
                    agent-shell-agent-configs))))
    (setq codex-acp-agent-shell--config-source nil)
    (advice-remove 'agent-shell--start #'codex-acp-agent-shell--start)))

;;;###autoload
(defun codex-acp-agent-shell-start ()
  "Start an Agent Shell conversation using local Codex and Emacs tools."
  (interactive)
  (codex-acp-agent-shell-mode 1)
  (agent-shell-start :config (codex-acp-agent-shell-make-config)))

(defun codex-acp-agent-shell-unload-function ()
  "Release clients and remove startup compatibility before unloading."
  (codex-acp-agent-shell-mode -1)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (local-variable-p 'codex-acp-agent-shell--client)
        (codex-acp-agent-shell--shutdown)
        (remove-hook 'kill-buffer-hook
                     #'codex-acp-agent-shell--shutdown t))))
  nil)

(provide 'codex-acp-agent-shell)
;;; codex-acp-agent-shell.el ends here
