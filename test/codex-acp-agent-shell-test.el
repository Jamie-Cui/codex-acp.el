;;; codex-acp-agent-shell-test.el --- Agent Shell integration regressions -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2026 Jamie Cui
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Exercise the real frontend with an in-memory Codex transport.  No model,
;; CLI, network connection or remote workspace is needed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'codex-acp-agent-shell)

(defconst codex-acp-agent-shell-test--available (require 'agent-shell nil t))

(defun codex-acp-agent-shell-test--client (buffer)
  "Make a process-backed test adapter for BUFFER without starting Codex."
  (let* ((directory (buffer-local-value 'default-directory buffer))
         (state (codex-acp--state-create :directory directory
                                        :local-directory directory))
         (process (make-pipe-process :name "codex-acp-agent-shell-test"
                                     :noquery t))
         (client (acp-make-client
                  :context-buffer buffer :command "codex"
                  :request-sender #'codex-acp--request
                  :notification-sender #'codex-acp--send-notification
                  :response-sender #'codex-acp--response)))
    (map-put! client :command nil)
    (map-put! client :process process)
    (push (cons :codex-acp-state state) client)
    (setf (codex-acp--state-client state) client
          (codex-acp--state-process state) process)
    client))

(defmacro codex-acp-agent-shell-test--with-buffer (&rest body)
  "Run BODY in a conversation buffer with normal buffer cleanup hooks."
  (declare (indent 0) (debug t))
  `(let ((buffer (generate-new-buffer " *Codex Emacs test*")))
     (unwind-protect
         (with-current-buffer buffer ,@body)
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defmacro codex-acp-agent-shell-test--with-mode (&rest body)
  "Run BODY with isolated configuration and remove integration afterward."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless codex-acp-agent-shell-test--available)
     (let ((agent-shell-agent-configs nil)
           (codex-acp-agent-shell--config-source nil))
       (unwind-protect
           (progn
             (codex-acp-agent-shell-mode 1)
             ,@body)
         (codex-acp-agent-shell-mode -1)))))

(ert-deftest codex-acp-agent-shell-test-registration-is-opt-in-and-reversible ()
  (skip-unless codex-acp-agent-shell-test--available)
  (let ((agent-shell-agent-configs '(existing-agent))
        (agent-shell-preferred-agent-config 'codex))
    (unwind-protect
        (cl-letf (((symbol-function 'codex-acp-make-client)
                   (lambda (&rest _) (ert-fail "Configuration started Codex"))))
          (codex-acp-agent-shell-mode 1)
          (codex-acp-agent-shell-mode 1)
          (should (equal agent-shell-agent-configs
                         '(codex-acp-agent-shell-make-config existing-agent)))
          (should (eq agent-shell-preferred-agent-config 'codex))
          (should (eq (map-elt (codex-acp-agent-shell-make-config) :identifier)
                      'codex-emacs)))
      (codex-acp-agent-shell-mode -1))
    (should (equal agent-shell-agent-configs '(existing-agent)))
    (should-not (advice-member-p #'codex-acp-agent-shell--start
                                'agent-shell--start))))

(ert-deftest codex-acp-agent-shell-test-callable-registry-stays-dynamic ()
  (skip-unless codex-acp-agent-shell-test--available)
  (let* ((entries '(first))
         (source (lambda () entries))
         (agent-shell-agent-configs source))
    (unwind-protect
        (progn
          (codex-acp-agent-shell-mode 1)
          (codex-acp-agent-shell-mode 1)
          (should (equal (funcall agent-shell-agent-configs)
                         '(codex-acp-agent-shell-make-config first)))
          (setq entries '(second))
          (should (equal (funcall agent-shell-agent-configs)
                         '(codex-acp-agent-shell-make-config second))))
      (codex-acp-agent-shell-mode -1))
    (should (eq agent-shell-agent-configs source))))

(ert-deftest codex-acp-agent-shell-test-probes-reuse-and-restart-replaces-client ()
  (codex-acp-agent-shell-test--with-mode
    (let ((count 0) clients)
      (cl-letf (((symbol-function 'codex-acp-make-client)
                 (lambda (buffer &optional _scope)
                   (cl-incf count)
                   (let ((client (codex-acp-agent-shell-test--client buffer)))
                     (push client clients)
                     client))))
        (unwind-protect
            (codex-acp-agent-shell-test--with-buffer
              (let ((first (codex-acp-agent-shell--make-client (current-buffer))))
                (should (eq first (codex-acp-agent-shell--make-client
                                   (current-buffer))))
                (should (= count 1))
                (should-not (map-elt first :command))
                (acp-shutdown :client first)
                (let ((second (codex-acp-agent-shell--make-client (current-buffer))))
                  (should (= count 2))
                  (should-not (eq first second))
                  (should (codex-acp-agent-shell--live-p second))))
              (kill-buffer (current-buffer)))
          (dolist (client clients)
            ;; The explicit buffer kill must have run our cleanup hook.
            (should-not (codex-acp-agent-shell--live-p client))
            (acp-shutdown :client client)))))))

(ert-deftest codex-acp-agent-shell-test-startup-failure-and-quit-release-client ()
  (codex-acp-agent-shell-test--with-mode
    (dolist (condition '(error quit))
      (let (client caught)
        (cl-letf (((symbol-function 'codex-acp-make-client)
                   (lambda (buffer &optional _scope)
                     (setq client (codex-acp-agent-shell-test--client buffer)))))
          (codex-acp-agent-shell-test--with-buffer
            (condition-case err
                (codex-acp-agent-shell--start
                 (lambda (&rest _)
                   (codex-acp-agent-shell--make-client (current-buffer))
                   (should (stringp (executable-find nil t)))
                   (should-not (executable-find "codex-acp-nonexistent-command"))
                   (signal condition '("Startup interrupted")))
                 :config (codex-acp-agent-shell-make-config))
              ((error quit) (setq caught (car err))))
            (should (eq caught condition))
            (should-not (codex-acp-agent-shell--live-p client))
            (should-not (map-elt client :command))
            (should-error (executable-find nil))))))))

(ert-deftest codex-acp-agent-shell-test-other-agents-keep-executable-validation ()
  (codex-acp-agent-shell-test--with-mode
    (codex-acp-agent-shell-test--with-buffer
      (let ((codex-acp-agent-shell--client
             (codex-acp-agent-shell-test--client (current-buffer))))
        (unwind-protect
            (should-error
             (codex-acp-agent-shell--start
              (lambda (&rest _) (executable-find nil))
              :config '((:identifier . codex))))
          (codex-acp-agent-shell--shutdown))))))

(ert-deftest codex-acp-agent-shell-test-tramp-directory-survives-path-resolver ()
  (codex-acp-agent-shell-test--with-mode
    (codex-acp-agent-shell-test--with-buffer
      (setq default-directory "/ssh:codex-test@invalid:/workspace/project/")
      (setq-local agent-shell-path-resolver-function #'file-local-name)
      (cl-letf (((symbol-function 'codex-acp-make-client)
                 #'codex-acp-agent-shell-test--client))
        (let* ((client (codex-acp-agent-shell--make-client (current-buffer)))
               (state (map-elt client :codex-acp-state))
               requested)
          (setf (codex-acp--state-initialized state) t)
          (cl-letf (((symbol-function 'codex-acp--rpc)
                     (lambda (_state method &rest _)
                       (setq requested method))))
            (codex-acp--request
             :client client
             :request (acp-make-session-new-request
                       :cwd (agent-shell--resolve-path default-directory))
             :on-failure (lambda (err) (ert-fail err))))
          (should (equal requested "thread/start"))
          (should (equal (codex-acp--state-directory state)
                         "/ssh:codex-test@invalid:/workspace/project/")))))))

(ert-deftest codex-acp-agent-shell-test-real-frontend-initializes-one-client ()
  (codex-acp-agent-shell-test--with-mode
    (let ((agent-shell-session-strategy 'new)
          (agent-shell-transcript-file-path-function nil)
          (agent-shell-show-welcome-message nil)
          (agent-shell-chat-mode-enabled nil)
          (agent-shell-mode-hook nil)
          (count 0)
          buffer client methods)
      (cl-letf (((symbol-function 'codex-acp-make-client)
                 (lambda (context &optional _scope)
                   (cl-incf count)
                   (setq client (codex-acp-agent-shell-test--client context))))
                ((symbol-function 'codex-acp--write) #'ignore)
                ((symbol-function 'codex-acp--title-ready) #'ignore)
                ((symbol-function 'codex-acp--refresh-skills) #'ignore)
                ((symbol-function 'agent-shell--display-buffer) #'ignore)
                ((symbol-function 'codex-acp--rpc)
                 (lambda (_state method _params success &rest _)
                   (push method methods)
                   (funcall success
                            (pcase method
                              ("initialize" nil)
                              ("thread/start" '((thread . ((id . "test-thread")))
                                                (model . "test-model")))
                              ("model/list" '((data . [((model . "test-model")
                                                       (displayName . "Test"))]))))))))
        (unwind-protect
            (save-window-excursion
              (setq buffer (agent-shell-start
                            :config (codex-acp-agent-shell-make-config)))
              (with-current-buffer buffer
                (should (= count 1))
                (should (eq (map-elt agent-shell--state :client) client))
                (should (equal (map-nested-elt agent-shell--state '(:session :id))
                               "test-thread"))
                (should-not (map-elt agent-shell--state :active-requests))
                (should-not (map-elt client :command))
                (should (equal (reverse methods)
                               '("initialize" "thread/start" "model/list")))
                (let (failure)
                  (agent-shell--send-request
                   :state agent-shell--state :client client
                   :request '((:method . "unsupported"))
                   :on-failure (lambda (err raw)
                                 (should-not raw)
                                 (setq failure err)))
                  (should (string-match-p "Unsupported ACP method"
                                          (map-elt failure 'message)))
                  (should-not (map-elt agent-shell--state :active-requests)))))

          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (set-buffer-modified-p nil)
              (let ((kill-buffer-query-functions nil))
                (kill-buffer buffer))))
          (when client
            (should-not (codex-acp-agent-shell--live-p client))
            (acp-shutdown :client client)))))))

(provide 'codex-acp-agent-shell-test)
;;; codex-acp-agent-shell-test.el ends here
