;;; tramp-async-test.el --- Async command contract regressions -*- lexical-binding: t; no-byte-compile: t -*-

;; Loaded by lisp/tools/test.sh.  For a focused offline run, initialize installed
;; packages, add the package directory to load-path, load this file and run
;; ert-run-tests-batch-and-exit in a disposable batch Emacs, never the live UI.

(require 'ert)
(require 'ert-x)
(require 'tramp-async)
(require 'envrc)
(require 'dired-x)
(require 'apheleia)
(defvar denote-dired-directories nil)

(defmacro tramp-async-test--enabled (&rest body)
  "Run BODY with adapters enabled in an isolated batch Emacs."
  (declare (indent 0))
  `(let ((tramp-async--workers (make-hash-table :test #'equal)))
     (unwind-protect
         (progn (tramp-async-files-mode 1) ,@body)
       (tramp-async-files-mode -1)
       (tramp-async-shutdown))))

(defun tramp-async-test--job ()
  "Return a settled fake request suitable for cancellation in fixture cleanup."
  (tramp-async--request-create :done t))

(defun tramp-async-test--snapshot (name &optional directory text)
  "Create observed fixture metadata for NAME, DIRECTORY and TEXT."
  (let* ((attrs (list (and directory t) 1 1000 1000 '(0 0) '(0 0) '(0 0)
                      0 (if directory "drwxr-xr-x" "-rwxr-xr-x") nil 1 1))
         (names (delete-dups (list name (file-name-directory name))))
         (metadata
          (mapcar (lambda (file)
                    (cons file (list (cons 'file-attributes attrs)
                                     (cons 'file-truename file) (cons 'file-exists-p t)
                                     (cons 'file-readable-p t) (cons 'file-writable-p t)
                                     (cons 'file-regular-p (not directory))
                                     (cons 'file-directory-p (or directory (not (equal name file))))
                                     (cons 'file-symlink-p nil) (cons 'file-modes #o755)
                                     (cons 'file-name-case-insensitive-p nil) (cons 'identity '(1 . 1))))) names)))
    (list :name name :path name :directory directory :metadata metadata
          :attributes attrs :parent-exists t :writable t :locals nil
          :coding 'utf-8-unix :switches "-al" :case-fold (mapcar (lambda (x) (cons x nil)) names)
          :text (or text (if directory (concat "  " name ":\n  total 0\n") "hello\n"))
          :version '(fixture))))

(ert-deftest tramp-async-test-envrc-direct-call-retains-native-contract ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (setq default-directory "/ssh:test@invalid:/project/" tramp-async-file-mode t)
      (let (seen)
        (cl-letf (((symbol-function 'tramp-async-call) (lambda (&rest _) (ert-fail "Unexpected job"))))
          (should (eq 'native (tramp-async-files--envrc-a
                              (lambda (force) (setq seen force) 'native) t)))
          (should seen))))))

(ert-deftest tramp-async-test-environment-ready-before-dired-hooks ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let* ((remote "/ssh:test@invalid:/project/")
             (snapshot (append '(:envrc (:directory "/ssh:test@invalid:/project/"
                                        :result (("TRAMP_TEST_ENV" . "ready"))))
                               (tramp-async-test--snapshot remote t)))
             (dired-after-readin-hook nil)
             (dired-mode-hook (list (lambda ()
                                     (should (equal "ready" (getenv-internal "TRAMP_TEST_ENV" tramp-remote-process-environment))))))
             (change-major-mode-after-body-hook (list (lambda () (envrc-mode 1))))
             (after-change-major-mode-hook nil))
        (setq default-directory remote tramp-async-files--state (tramp-async-files--state-create :file remote))
        (tramp-async-file-mode 1)
        (cl-letf (((symbol-function 'envrc--find-env-dir) (lambda () (ert-fail "Parent envrc I/O")))
                  ((symbol-function 'tramp-async-call) (lambda (&rest _) (ert-fail "Late envrc job"))))
          (tramp-async-dired--install snapshot t)
          (should (eq envrc--status 'on))
          (should-not (tramp-async-files--state-error tramp-async-files--state)))))))

(ert-deftest tramp-async-test-environment-ready-before-file-hooks ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let* ((file "/ssh:test@invalid:/project/a.txt")
             (snapshot (append '(:envrc (:directory "/ssh:test@invalid:/project/"
                                        :result (("TRAMP_TEST_ENV" . "ready"))))
                               (tramp-async-test--snapshot file)))
             (auto-save-default nil) (enable-local-variables nil)
             (change-major-mode-after-body-hook (list (lambda () (envrc-mode 1))))
             (text-mode-hook (list (lambda ()
                                    (should (equal "ready" (getenv-internal "TRAMP_TEST_ENV" tramp-remote-process-environment))))))
             (find-file-hook nil))
        (setq tramp-async-files--state (tramp-async-files--state-create :file file))
        (tramp-async-file-mode 1)
        (cl-letf (((symbol-function 'envrc--find-env-dir) (lambda () (ert-fail "Parent envrc I/O"))))
          (tramp-async-files--install-visit tramp-async-files--state snapshot nil t))
        (setq buffer-file-name nil)))))

(ert-deftest tramp-async-test-guard-does-not-rebind-global-handler ()
  (tramp-async-test--enabled
    (let ((other (generate-new-buffer " *tramp-test-other*")) timer observed native)
      (unwind-protect
          (with-temp-buffer
            (let ((handler (symbol-function 'tramp-file-name-handler)))
              (tramp-async-files--without-remote-io
                (setq timer (run-at-time 0 nil
                             (lambda ()
                               (with-current-buffer other
                                 (tramp-async-files--io-a (lambda (&rest _) (setq native t))
                                                          'file-exists-p "/ssh:test@invalid:/other")
                                 (setq observed (eq handler (symbol-function 'tramp-file-name-handler)))))))
                (sleep-for 0.05))
              (should native) (should observed)))
        (when timer (cancel-timer timer)) (kill-buffer other)))))

(ert-deftest tramp-async-test-observed-metadata-and-unknown-values ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let ((file "/ssh:test@invalid:/project/tool"))
        (setq tramp-async-files--state (tramp-async-files--state-create :snapshot (tramp-async-test--snapshot file)))
        (tramp-async-files--without-remote-io
          (should (= #o755 (file-modes file)))
          (should-not (file-symlink-p file))
          (should-error (file-modes "/ssh:test@invalid:/unknown") :type 'tramp-async-files-sync-io)
          (should-error (file-equal-p file "/ssh:test@invalid:/unknown") :type 'tramp-async-files-sync-io))))))

(ert-deftest tramp-async-test-worker-preserves-real-permissions-and-links ()
  (let* ((dir (make-temp-file "tramp-metadata-test-" t))
         (file (expand-file-name "executable" dir)) (link (expand-file-name "link" dir)))
    (unwind-protect
        (progn
          (write-region "test\n" nil file nil 'silent) (set-file-modes file #o751)
          (make-symbolic-link "executable" link)
          (let ((properties (cdr (assoc link (tramp-async-files-worker--metadata (list link))))))
            (should (= #o751 (cdr (assq 'file-modes properties))))
            (should (equal "executable" (cdr (assq 'file-symlink-p properties))))
            (should (equal "executable" (car (cdr (assq 'file-attributes properties)))))))
      (delete-directory dir t))))

(defvar tramp-async-test-hook nil)
(ert-deftest tramp-async-test-hook-failure-is-recorded-and-reported ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (setq tramp-async-files--state (tramp-async-files--state-create))
      (let ((tramp-async-test-hook (list (lambda () (error "Fixture failure")))))
        (should-error (tramp-async-files--run-hook 'tramp-async-test-hook) :type 'tramp-async-files-hook-error)
        (should (eq 'error (tramp-async-files--state-phase tramp-async-files--state)))
        (should (string-match-p "Fixture failure" (tramp-async-files--state-error tramp-async-files--state)))))))

(ert-deftest tramp-async-test-native-dired-apis-are-not-deferred ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (setq default-directory "/ssh:test@invalid:/project/" major-mode 'dired-mode tramp-async-file-mode t)
      (cl-letf (((symbol-function 'tramp-async-call) (lambda (&rest _) (ert-fail "Unexpected job"))))
        (dolist (adapter '(tramp-async-dired--find-a tramp-async-dired--find-window-a
                           tramp-async-dired--up-a tramp-async-dired--revert-a
                           tramp-async-dired--mark-omitted-a tramp-async-dired--subtree-a
                           tramp-async-dired--subtree-toggle-a))
          (should (eq 'native (funcall adapter (lambda (&rest _) 'native)))))
        (should (eq 'native (tramp-async-dired--insert-a (lambda (&rest _) 'native) "sub")))
        (should (eq 'native (tramp-async-dired--maybe-insert-a (lambda (&rest _) 'native) "sub")))
        (should (eq 'native (tramp-async-files--revert-a (lambda (&rest _) 'native) t t t)))))))

(ert-deftest tramp-async-test-native-revert-function-stays-synchronous ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let ((revert-buffer-function #'ignore))
        (tramp-async-file-mode 1)
        (should (eq revert-buffer-function #'ignore)))
      (setq major-mode 'dired-mode default-directory "/ssh:test@invalid:/project/" dired-directory default-directory)
      (tramp-async-dired--adopt)
      (should (eq revert-buffer-function #'dired-revert)))))

(ert-deftest tramp-async-test-interactive-omitted-marks-wait-for-listing ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let* ((remote "/ssh:test@invalid:/project/") callback marked installed (job (tramp-async-test--job))
             (dired-before-readin-hook nil))
        (setq default-directory remote major-mode 'dired-mode dired-directory remote
              dired-subdir-alist (list (cons remote (point-marker))) dired-actual-switches "-al"
              tramp-async-files--state (tramp-async-files--state-create :file remote))
        (tramp-async-file-mode 1)
        (cl-letf (((symbol-function 'tramp-async-call) (lambda (_d _f _a cb &rest _) (setq callback cb) job))
                  ((symbol-function 'tramp-async-dired--install) (lambda (&rest _) (should-not dired-omit-mode) (setq installed t)))
                  ((symbol-function 'dired-omit-case-fold-p) (lambda (&rest _) nil))
                  ((symbol-function 'dired-mark-unmarked-files) (lambda (&rest _) (should installed) (setq marked t))))
          (call-interactively #'dired-mark-omitted)
          (should-not marked)
          (funcall callback (list :status 'ok :value (tramp-async-test--snapshot remote t)))
          (should marked))))))

(ert-deftest tramp-async-test-native-omitted-marks-stay-synchronous ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let (refreshed)
        (cl-letf (((symbol-function 'revert-buffer) (lambda (&rest _) (setq refreshed t)))
                  ((symbol-function 'dired-omit-case-fold-p) (lambda (&rest _) nil))
                  ((symbol-function 'dired-mark-unmarked-files) (lambda (&rest _) (should refreshed))))
          (dired-mark-omitted))))))

(ert-deftest tramp-async-test-synchronous-save-updates-next-worker-version ()
  (with-temp-buffer
    (setq buffer-file-name "/ssh:test@invalid:/project/a.txt"
          tramp-async-files--state (tramp-async-files--state-create :version '(old)))
    (let ((file-name-handler-alist nil)) (set-visited-file-modtime '(100 200)))
    (tramp-async-files--basic-save-a (lambda (&rest _) (set-buffer-modified-p nil)))
    (should (equal (tramp-async-files--state-version tramp-async-files--state) (list :mtime (visited-file-modtime))))
    (setq buffer-file-name nil)))

(ert-deftest tramp-async-test-denote-local-roots-do-not-probe-remote-host ()
  (with-temp-buffer
    (setq tramp-async-file-mode t default-directory "/ssh:test@invalid:/project/")
    (let ((denote-dired-directories '("/local/notes/")))
      (tramp-async-files--denote-a (lambda () (ert-fail "Cross-host Denote lookup"))))))

(ert-deftest tramp-async-test-apheleia-followup-awaits-worker-save ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let* (format-callback save-callback posted native-saved
            (apheleia-mode t) (apheleia-format-after-save-in-progress nil)
            (apheleia-post-format-hook (list (lambda () (setq posted t)))))
        (setq buffer-file-name "/ssh:test@invalid:/project/a.go"
              tramp-async-file-mode t tramp-async-files--state (tramp-async-files--state-create))
        (cl-letf (((symbol-function 'apheleia--get-formatters) (lambda (&rest _) '(fixture)))
                  ((symbol-function 'apheleia-format-buffer)
                   (lambda (_formatters _success &rest options) (setq format-callback (plist-get options :callback))))
                  ((symbol-function 'tramp-async-files-save-buffer)
                   (lambda (&rest _) (should tramp-async-files--save-followup)
                     (setq save-callback tramp-async-files--save-completion)))
                  ((symbol-function 'save-buffer) (lambda (&rest _) (setq native-saved t))))
          (apheleia-format-after-save)
          (should tramp-async-files--format-token)
          (funcall format-callback :error nil)
          (should-not posted) (should-not native-saved)
          (should tramp-async-files--format-token)
          (funcall save-callback '(:status ok))
          (should posted) (should-not tramp-async-files--format-token))
        (setq buffer-file-name nil)))))

(ert-deftest tramp-async-test-batch-waits-for-formatting ()
  (with-temp-buffer
    (let* ((batch (tramp-async-batch--create :buffers (list (current-buffer))))
           (tramp-async-batch--active batch) finished)
      (setq tramp-async-files--format-token 'pending)
      (cl-letf (((symbol-function 'tramp-async-batch--finish) (lambda (_) (setq finished t))))
        (tramp-async-batch--poll batch) (should-not finished)
        (setq tramp-async-files--format-token nil)
        (tramp-async-batch--poll batch) (should finished)))))

(ert-deftest tramp-async-envrc-worker-honors-direnv-permission-and-json ()
  (dolist (allowed '(0 1 2 nil 7))
    (let (commands)
      (cl-letf (((symbol-function 'locate-dominating-file) (lambda (&rest _) "/tmp/"))
                ((symbol-function 'process-file)
                 (lambda (program _in destination _display &rest arguments)
                   (should (equal program "direnv"))
                   (should (equal destination '(t nil)))
                   (push arguments commands)
                   (insert
                    (if (equal arguments '("status" "--json"))
                        (format "{\"state\":{\"foundRC\":{\"allowed\":%s}}}" (or allowed "null"))
                      "{\"TEST_VALUE\":\"ok\",\"UNSET_VALUE\":null}"))
                   0)))
        (if (eq allowed 7)
            (should-error (tramp-async-files-worker-envrc "/tmp/" "direnv"))
          (let ((result (plist-get (tramp-async-files-worker-envrc "/tmp/" "direnv") :result)))
            (pcase allowed
              (0 (should (equal (cdr (assoc "TEST_VALUE" result)) "ok"))
                 (should (assoc "UNSET_VALUE" result)))
              ((or 1 2) (should (eq result 'denied)))
              ('nil (should (eq result 'none))))))
        (should (equal (reverse commands)
                       (if (eq allowed 0) '(("status" "--json") ("export" "json"))
                         '(("status" "--json")))))))))

(ert-deftest tramp-async-envrc-worker-no-environment-skips-commands ()
  (cl-letf (((symbol-function 'locate-dominating-file) (lambda (&rest _) nil))
            ((symbol-function 'process-file) (lambda (&rest _) (ert-fail "Unexpected direnv invocation"))))
    (should (equal (tramp-async-files-worker-envrc "/tmp/" "direnv") '(:directory nil :result none)))))

(ert-deftest tramp-async-envrc-malformed-output-is-not-logged ()
  (cl-letf (((symbol-function 'process-file)
             (lambda (&rest _) (insert "{PRIVATE_VALUE: broken}") 0)))
    (let ((error-data (should-error (tramp-async-files-worker--direnv-json "direnv" "export" "json"))))
      (should (equal (error-message-string error-data) "Invalid JSON from direnv")))))

(ert-deftest tramp-async-envrc-real-worker-keeps-parent-responsive ()
  (let* ((directory (make-temp-file "tramp-async-envrc-test-" t))
         (program (expand-file-name "direnv-test" directory))
         (tramp-async--workers (make-hash-table :test #'equal))
         (ticks 0) result timer)
    (unwind-protect
        (progn
          (write-region "# Test fixture only\n" nil (expand-file-name ".envrc" directory) nil 'silent)
          (write-region "#!/bin/sh\ncase \"$1\" in\nstatus) printf '%s' '{\"state\":{\"foundRC\":{\"allowed\":0}}}' ;;\nexport) sleep 0.2; printf '%s' '{\"TEST_VALUE\":\"ok\"}' ;;\nesac\n" nil program nil 'silent)
          (set-file-modes program #o700)
          (setq timer (run-at-time 0 0.02 (lambda () (cl-incf ticks))))
          (tramp-async-call directory #'tramp-async-files-worker-envrc (list directory program)
                            (lambda (reply) (setq result reply)) :timeout 5)
          (let ((deadline (+ (float-time) 7)))
            (while (and (not result) (< (float-time) deadline)) (accept-process-output nil 0.02)))
          (should (eq (plist-get result :status) 'ok))
          (should (equal (cdr (assoc "TEST_VALUE" (plist-get (plist-get result :value) :result))) "ok"))
          (should (> ticks 5)))
      (when timer (cancel-timer timer))
      (tramp-async-shutdown)
      (delete-directory directory t))))


(ert-deftest tramp-async-test-native-visit-save-and-mutations-delegate ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (setq buffer-file-name "/ssh:test@invalid:/project/a.txt"
            default-directory "/ssh:test@invalid:/project/" major-mode 'dired-mode)
      (cl-letf (((symbol-function 'tramp-async-call) (lambda (&rest _) (ert-fail "Unexpected async job"))))
        (should (eq 'native (tramp-async-files--find-a (lambda (&rest _) 'native) buffer-file-name)))
        (dolist (fn '(tramp-async-files--save-a tramp-async-batch--save-a
                      tramp-async-dired-ops--copy-a tramp-async-dired-ops--rename-a
                      tramp-async-dired-ops--delete-a tramp-async-dired-ops--flagged-a))
          (should (eq 'native (funcall fn (lambda (&rest _) 'native))))))
      (setq buffer-file-name nil))))

(ert-deftest tramp-async-test-selection-does-not-intercept-unrelated-saves ()
  (with-temp-buffer
    (insert "fixture")
    (let ((buffer (current-buffer)) saved)
      (cl-letf (((symbol-function 'files--buffers-needing-to-be-saved) (lambda (_) (list buffer)))
                ((symbol-function 'save-buffer) (lambda (&rest _) (setq saved t)))
                ((symbol-function 'map-y-or-n-p)
                 (lambda (_prompt action _buffers &rest _)
                   (save-buffer)
                   (funcall action buffer))))
        (should (equal (list buffer) (tramp-async-batch--select nil nil)))
        (should saved)))))

(ert-deftest tramp-async-test-save-ack-preserves-newer-edits ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let* ((file "/ssh:test@invalid:/project/a.txt")
             (snapshot (tramp-async-test--snapshot file)) callback (job (tramp-async-test--job))
             (before-save-hook nil) (after-save-hook nil))
        (insert "first\n")
        (setq buffer-file-name file buffer-file-coding-system 'utf-8-unix
              tramp-async-files--state (tramp-async-files--state-create :file file :snapshot snapshot))
        (tramp-async-file-mode 1)
        (cl-letf (((symbol-function 'tramp-async-call) (lambda (_d _f _a cb &rest _) (setq callback cb) job)))
          (tramp-async-files-save-buffer)
          (insert "newer\n")
          (funcall callback (list :status 'ok :value snapshot))
          (should (buffer-modified-p))
          (should (string-match-p "newer" (buffer-string))))
        (set-buffer-modified-p nil)
        (setq buffer-file-name nil buffer-file-truename nil)))))

(ert-deftest tramp-async-test-save-hook-error-settles-completion ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let* ((file "/ssh:test@invalid:/project/a.txt") (snapshot (tramp-async-test--snapshot file))
             callback result (job (tramp-async-test--job))
             (tramp-async-files--save-completion (lambda (reply) (setq result reply)))
             (before-save-hook nil) (after-save-hook (list (lambda () (error "Hook fixture failure")))))
        (insert "data\n")
        (setq buffer-file-name file buffer-file-coding-system 'utf-8-unix
              tramp-async-files--state (tramp-async-files--state-create :file file :snapshot snapshot))
        (tramp-async-file-mode 1)
        (cl-letf (((symbol-function 'tramp-async-call) (lambda (_d _f _a cb &rest _) (setq callback cb) job))
                  ((symbol-function 'display-warning) #'ignore))
          (tramp-async-files-save-buffer)
          (funcall callback (list :status 'ok :value snapshot))
          (should (eq (plist-get result :status) 'error))
          (should-not (buffer-modified-p))
          (should (eq 'error (tramp-async-files--state-phase tramp-async-files--state))))
        (setq buffer-file-name nil)))))

(ert-deftest tramp-async-test-format-error-releases-batch-wait ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let ((apheleia-mode t) (apheleia-format-after-save-in-progress nil) callback)
        (setq buffer-file-name "/ssh:test@invalid:/project/a.go" tramp-async-file-mode t
              tramp-async-files--state (tramp-async-files--state-create))
        (cl-letf (((symbol-function 'apheleia--get-formatters) (lambda (&rest _) '(fixture)))
                  ((symbol-function 'apheleia-format-buffer)
                   (lambda (_f _s &rest args) (setq callback (plist-get args :callback))))
                  ((symbol-function 'display-warning) #'ignore))
          (apheleia-format-after-save)
          (funcall callback :error '(error "Formatter fixture failure"))
          (should-not tramp-async-files--format-token)
          (should (eq 'error (tramp-async-files--state-phase tramp-async-files--state))))
        (setq buffer-file-name nil)))))

(defun tramp-async-test--await (predicate)
  "Allow event processing until PREDICATE succeeds, bounded to ten seconds."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall predicate))))

(ert-deftest tramp-async-test-real-worker-visit-save-conflict ()
  (tramp-async-test--enabled
    (let* ((dir (make-temp-file "tramp-save-test-" t)) (file (expand-file-name "a.txt" dir)) result snapshot)
      (unwind-protect
          (progn
            (write-region "original\n" nil file nil 'silent)
            (tramp-async-call dir #'tramp-async-files-worker-visit (list file 4096)
                              (lambda (reply) (setq result reply)) :mutating nil)
            (tramp-async-test--await (lambda () result))
            (should (eq (plist-get result :status) 'ok))
            (setq snapshot (plist-get result :value) result nil)
            (write-region "external edit\n" nil file nil 'silent)
            (tramp-async-call dir #'tramp-async-files-worker-save
                              (list file "my edit\n" 'utf-8-unix (plist-get snapshot :version) 4096 nil nil nil)
                              (lambda (reply) (setq result reply)))
            (tramp-async-test--await (lambda () result))
            (should (eq 'tramp-async-files-conflict (car (plist-get result :error))))
            (should (equal "external edit\n" (with-temp-buffer (insert-file-contents file) (buffer-string)))))
        (tramp-async-shutdown) (delete-directory dir t)))))

(ert-deftest tramp-async-test-real-worker-dired-copy-rename-delete ()
  (tramp-async-test--enabled
    (let* ((dir (make-temp-file "tramp-dired-test-" t))
           (source (expand-file-name "source unicode-文件.txt" dir))
           (copy (expand-file-name "copy.txt" dir)) (renamed (expand-file-name "renamed.txt" dir))
           result events)
      (unwind-protect
          (progn
            (write-region "fixture\n" nil source nil 'silent)
            (write-region "overwrite me\n" nil copy nil 'silent)
            (dolist (spec (list (list 'copy source copy) (list 'rename copy renamed) (list 'delete renamed nil)))
              (setq result nil events nil)
              (tramp-async-call
               dir #'tramp-async-dired-ops-worker-run
               (list (car spec) (list (cadr spec)) (nth 2 spec) '((dired-backup-overwrite . nil)) nil)
               (lambda (reply) (setq result reply))
               :event-handler (lambda (event)
                                (push event events)
                                (if (memq (plist-get event :kind) '(read-char-choice read-answer)) ?y t)))
              (tramp-async-test--await (lambda () result))
              (should (eq (plist-get result :status) 'ok))
              (should (cl-find 'ok events :key (lambda (event) (plist-get (plist-get event :item) :status))))
              (pcase (car spec)
                ('copy (should (equal "fixture\n" (with-temp-buffer (insert-file-contents copy) (buffer-string)))))
                ('rename (should (file-exists-p renamed)) (should-not (file-exists-p copy)))
                ('delete (should-not (file-exists-p renamed)))))
            (should (file-exists-p source)))
        (tramp-async-shutdown) (delete-directory dir t)))))

(ert-deftest tramp-async-test-format-followup-quit-releases-batch-wait ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let ((apheleia-mode t) (apheleia-format-after-save-in-progress nil) callback)
        (setq buffer-file-name "/ssh:test@invalid:/project/a.go" tramp-async-file-mode t
              tramp-async-files--state (tramp-async-files--state-create))
        (cl-letf (((symbol-function 'apheleia--get-formatters) (lambda (&rest _) '(fixture)))
                  ((symbol-function 'apheleia-format-buffer)
                   (lambda (_f _s &rest args) (setq callback (plist-get args :callback))))
                  ((symbol-function 'tramp-async-files-save-buffer) (lambda (&rest _) (signal 'quit nil)))
                  ((symbol-function 'display-warning) #'ignore))
          (apheleia-format-after-save)
          (funcall callback)
          (should-not tramp-async-files--format-token)
          (should (eq 'error (tramp-async-files--state-phase tramp-async-files--state))))
        (setq buffer-file-name nil)))))

(ert-deftest tramp-async-test-save-preserves-link-name-metadata ()
  (tramp-async-test--enabled
    (let* ((dir (make-temp-file "tramp-link-save-" t))
           (file (expand-file-name "target" dir)) (link (expand-file-name "link" dir)) result)
      (unwind-protect
          (progn
            (write-region "original\n" nil file nil 'silent)
            (make-symbolic-link "target" link)
            (let ((snapshot (tramp-async-files-worker-visit link 4096)))
              (tramp-async-call
               dir #'tramp-async-files-worker-save
               (list link "saved\n" 'utf-8-unix (plist-get snapshot :version) 4096
                     '((make-backup-files . nil) (file-preserve-symlinks-on-save . t)) nil nil)
               (lambda (reply) (setq result reply)))
              (tramp-async-test--await (lambda () result))
              (should (eq (plist-get result :status) 'ok))
              (let* ((saved (plist-get result :value))
                     (metadata (cdr (assoc link (plist-get saved :metadata)))))
                (should (equal link (plist-get saved :name)))
                (should (equal "target" (cdr (assq 'file-symlink-p metadata)))))))
        (tramp-async-shutdown) (delete-directory dir t)))))

(ert-deftest tramp-async-test-dired-refresh-reconciles-home-alias ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let* ((directory "/ssh:test@invalid:/home/test/")
             (alias "/ssh:test@invalid:~/")
             (snapshot
              (tramp-async-test--snapshot
               directory t
               (concat "  " directory ":\n  total 0\n"
                       "  -rw-r--r-- 1 test test 4 Jan  1  2026 keep.txt\n"
                       "  -rw-r--r-- 1 test test 4 Jan  1  2026 .hidden\n")))
             (dired-mode-hook nil) (dired-after-readin-hook nil)
             (change-major-mode-after-body-hook nil) (after-change-major-mode-hook nil)
             (dired-omit-verbose nil) (dired-omit-files "^\\.")
             (dired-omit-case-fold 'filesystem))
        (setq default-directory directory
              tramp-async-files--state (tramp-async-files--state-create :file directory))
        (tramp-async-file-mode 1)
        (tramp-async-dired--install (copy-tree snapshot) t)
        ;; Native Dired may abbreviate this variable while keeping an expanded
        ;; default-directory.  Refreshing that adopted buffer must reconcile it.
        (setq dired-directory alias)
        (dired-goto-file (concat directory "keep.txt"))
        (dired-mark 1)
        (dired-goto-file (concat directory "keep.txt"))
        (let ((dired-omit-mode t) (dired-after-readin-hook '(dired-omit-expunge)))
          (tramp-async-dired--install (copy-tree snapshot))
          (should (equal dired-directory directory))
          (should (equal (dired-get-filename) (concat directory "keep.txt")))
          (should (equal (dired-get-marked-files) (list (concat directory "keep.txt"))))
          (should-not (string-match-p "\\.hidden" (buffer-string))))
        (should-not (tramp-async-files--state-error tramp-async-files--state))))))

(defun tramp-async-test--dired-snapshot (directory &optional rows)
  "Build a listing for DIRECTORY with ROWS as (MODE NAME) pairs."
  (tramp-async-test--snapshot
   directory t
   (concat "  " directory ":\n  total 0\n"
           (mapconcat (lambda (row)
                        (format "  %s 1 test test 4 Jan  1  2026 %s\n" (car row) (cadr row)))
                      rows ""))))

(defun tramp-async-test--dired-install (snapshot &optional refresh)
  "Install fixture SNAPSHOT with optional REFRESH without user hooks."
  (let ((dired-mode-hook nil) (dired-after-readin-hook nil)
        (change-major-mode-after-body-hook nil) (after-change-major-mode-hook nil))
    (unless refresh
      (setq default-directory (plist-get snapshot :name)
            tramp-async-files--state (tramp-async-files--state-create :file default-directory))
      (tramp-async-file-mode 1))
    (tramp-async-dired--install (copy-tree snapshot) (not refresh))))

(ert-deftest tramp-async-test-dired-observed-home-and-case-rules ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let* ((home "/ssh:test@invalid:/home/test/") (alias "/ssh:test@invalid:~/")
             (snapshot (tramp-async-test--snapshot home t)))
        (setq snapshot (plist-put snapshot :aliases (list (cons alias home)))
              default-directory alias
              tramp-async-files--state (tramp-async-files--state-create :snapshot snapshot))
        (tramp-async-files--without-remote-io
          (should (equal (expand-file-name "sub/../file") (concat home "file")))
          (should (equal (expand-file-name (directory-file-name alias)) (directory-file-name home)))
          (should-not (file-name-case-insensitive-p alias))
          (should (file-equal-p alias home))
          (should (file-equal-p home alias))
          (dolist (unknown '("/ssh:test@invalid:~someone/" "/ssh:other@invalid:~/"
                             "/ssh:test@invalid:/home/test2/"))
            (should (equal unknown (expand-file-name unknown)))
            (should-error (file-name-case-insensitive-p unknown) :type 'tramp-async-files-sync-io)))))))

(ert-deftest tramp-async-test-dired-worker-keeps-links-and-normalizes-subdirectories ()
  (let* ((base (make-temp-file "tramp-path-test-" t))
         (real (expand-file-name "real" base)) (link (expand-file-name "link" base))
         (child (expand-file-name "child" real)) (dangling (expand-file-name "dangling" base)))
    (unwind-protect
        (progn
          (make-directory child t)
          (write-region "data" nil (expand-file-name "space name.txt" child) nil 'silent)
          (make-symbolic-link "real" link) (make-symbolic-link "missing" dangling)
          (let* ((name (file-name-as-directory link))
                 (snapshot (tramp-async-files-worker-dired name "-al" 65536 nil '("child/") '("child/"))))
            (should (equal name (plist-get snapshot :name)))
            (should (equal (file-name-as-directory (file-truename real)) (plist-get snapshot :path)))
            (should (assoc (concat name "child/") (plist-get snapshot :case-fold)))
            (should (assoc (concat name "child") (plist-get snapshot :subtrees)))
            (should (equal name (tramp-async-files--expand-aliases name nil (plist-get snapshot :aliases)))))
          (should-not (plist-get (tramp-async-files-worker-visit dangling 4096) :attributes))
          (should-error (tramp-async-files-worker-visit dangling 4096 nil nil "-al" nil t)))
      (delete-directory base t))))

(ert-deftest tramp-async-test-dired-reuse-preserves-logical-symlink-path ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let* ((name "/ssh:test@invalid:/home/test/link/")
             (target "/ssh:test@invalid:/srv/real/")
             (snapshot (tramp-async-test--dired-snapshot name)))
        (setq snapshot (plist-put snapshot :path target)
              snapshot (plist-put snapshot :aliases '(("/ssh:test@invalid:~/" . "/ssh:test@invalid:/home/test/"))))
        (tramp-async-test--dired-install snapshot)
        (should (eq (current-buffer) (tramp-async-files--visiting "/ssh:test@invalid:~/link/")))
        (should-not (tramp-async-files--visiting target))))))

(ert-deftest tramp-async-test-dired-up-root-and-unresolved-home ()
  (tramp-async-test--enabled
    (dolist (directory '("/ssh:test@invalid:/" "/ssh:test@invalid:~/"))
      (with-temp-buffer
        (let (requested)
          (tramp-async-test--dired-install (tramp-async-test--dired-snapshot directory))
          (cl-letf (((symbol-function 'tramp-async-dired)
                     (lambda (dir &rest _) (setq requested dir))))
            (call-interactively #'dired-up-directory))
          (if (equal directory "/ssh:test@invalid:/") (should-not requested)
            (should (equal requested "/ssh:test@invalid:~/../"))))))))

(ert-deftest tramp-async-test-dired-up-waits-and-selects-logical-child ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let* ((parent "/ssh:test@invalid:/home/test/") (dir (concat parent "link/"))
             (snapshot (tramp-async-test--dired-snapshot dir '(("drwxr-xr-x" ".") ("drwxr-xr-x" ".."))))
             callback requested)
        (tramp-async-test--dired-install (plist-put snapshot :path "/ssh:test@invalid:/srv/real/"))
        (cl-letf (((symbol-function 'tramp-async-dired)
                   (lambda (directory _switches _display cb) (setq requested directory callback cb))))
          (call-interactively #'dired-up-directory))
        (should (equal requested parent)) (should callback)
        (should (equal default-directory dir))
        (tramp-async-test--dired-install
         (tramp-async-test--dired-snapshot parent '(("lrwxrwxrwx" "link -> /srv/real"))))
        (funcall callback)
        (should (equal (dired-get-filename) (concat parent "link")))))))

(ert-deftest tramp-async-test-dired-insert-home-alias-and-refresh-state ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let* ((root "/ssh:test@invalid:/home/test/") (sub (concat root "sub/"))
             (alias "/ssh:test@invalid:~/sub/")
             (snapshot (tramp-async-test--dired-snapshot root '(("drwxr-xr-x" "sub"))))
             callback requested (job (tramp-async-test--job))
             (dired-before-readin-hook nil) (dired-after-readin-hook nil))
        (tramp-async-test--dired-install snapshot)
        (setq snapshot (plist-put snapshot :text
                                 (concat (plist-get snapshot :text) "\n"
                                         (plist-get (tramp-async-test--dired-snapshot sub '(("-rw-r--r--" "space name.txt"))) :text)))
              snapshot (plist-put snapshot :aliases (list (cons "/ssh:test@invalid:~/" root)))
              snapshot (plist-put snapshot :case-fold (list (cons root nil) (cons sub nil))))
        (cl-letf (((symbol-function 'tramp-async-call)
                   (lambda (_dir _fn args cb &rest _) (setq requested (nth 4 args) callback cb) job)))
          (funcall-interactively #'dired-insert-subdir alias)
          (should (equal (list alias) requested))
          (should-not (assoc sub dired-subdir-alist))
          (funcall callback (list :status 'ok :value (copy-tree snapshot))))
        (should-not (tramp-async-files--state-error tramp-async-files--state))
        (should (equal (dired-current-directory) sub))
        (should (dired-goto-file (concat alias "space name.txt")))
        (dired-mark 1) (dired-goto-file (concat sub "space name.txt"))
        (tramp-async-test--dired-install snapshot t)
        (should (equal (dired-get-filename) (concat sub "space name.txt")))
        (should (member (concat sub "space name.txt") (dired-get-marked-files)))
        (cl-letf (((symbol-function 'tramp-async-call) (lambda (&rest _) (ert-fail "Subdirectory is already inserted"))))
          (funcall-interactively #'dired-maybe-insert-subdir alias))
        (should (equal (dired-current-directory) sub))
        (cl-letf (((symbol-function 'tramp-async-dired) (lambda (&rest _) (ert-fail "Parent already in buffer"))))
          (call-interactively #'dired-up-directory))
        (should (equal (dired-get-filename) (concat root "sub")))
        (dired-goto-subdir sub) (dired-hide-subdir 1)
        (tramp-async-test--dired-install snapshot t)
        (should (member sub (dired-remember-hidden)))))))

(ert-deftest tramp-async-test-dired-goto-preserves-point-on-missing-entry ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (let ((root "/ssh:test@invalid:/project/"))
        (tramp-async-test--dired-install
         (tramp-async-test--dired-snapshot root '(("-rw-r--r--" "space name.txt"))))
        (should (dired-goto-file (concat root "space name.txt")))
        (let ((position (point)))
          (should-not (dired-goto-file (concat root "gone")))
          (should (= (point) position)))
        (should-error (dired-goto-file "relative"))))))

(ert-deftest tramp-async-test-dired-jump-relative-and-nonfile-buffers ()
  (tramp-async-test--enabled
    (with-temp-buffer
      (setq default-directory "/ssh:test@invalid:/project/")
      (let (requested callback display)
        (cl-letf (((symbol-function 'tramp-async-dired)
                   (lambda (dir &optional _switches show cb)
                     (setq requested dir callback cb display show))))
          (funcall-interactively #'dired-jump nil "a.txt")
          (should (equal requested default-directory)) (should callback)
          (funcall-interactively #'dired-jump-other-window)
          (should (equal requested default-directory)) (should-not callback)
          (should (eq display #'switch-to-buffer-other-window)))))))

(ert-deftest tramp-async-test-dired-subtree-expansion-refresh-and-up ()
  (require 'dired-subtree)
  (tramp-async-test--enabled
    (with-temp-buffer
      (let* ((root "/ssh:test@invalid:/project/") (child (concat root "child"))
             (snapshot (tramp-async-test--dired-snapshot root '(("drwxr-xr-x" "child"))))
             (listing (concat "total 0\n"
                              "drwxr-xr-x 1 test test 4 Jan  1  2026 .\n"
                              "drwxr-xr-x 1 test test 4 Jan  1  2026 ..\n"
                              "-rw-r--r-- 1 test test 4 Jan  1  2026 space name.txt\n"))
             callback args (job (tramp-async-test--job)))
        (tramp-async-test--dired-install snapshot)
        (dired-goto-file child)
        (cl-letf (((symbol-function 'tramp-async-call)
                   (lambda (_dir fn arguments cb &rest _)
                     (should (eq fn 'tramp-async-files-worker-subtree))
                     (setq callback cb args arguments) job)))
          (call-interactively #'dired-subtree-toggle)
          (should-not dired-subtree-overlays)
          (should (equal (car args) child))
          (should (equal (cadr args) dired-actual-switches))
          (funcall callback (list :status 'ok :value (list :text listing :case-fold (list (cons (concat child "/") nil))))))
        (should-not (tramp-async-files--state-error tramp-async-files--state))
        (should (= 1 (length dired-subtree-overlays)))
        (should (dired-goto-file (concat child "/space name.txt")))
        (should (equal (dired-current-directory) (concat child "/")))
        (let ((dired-after-readin-hook '(dired-subtree--after-readin))
              (refresh (plist-put (copy-tree snapshot) :subtrees (list (cons child listing)))))
          (cl-letf (((symbol-function 'tramp-async-call) (lambda (&rest _) (ert-fail "Refresh already has subtree listing"))))
            (tramp-async-dired--install refresh)))
        (should (equal (dired-get-filename) (concat child "/space name.txt")))
        (cl-letf (((symbol-function 'tramp-async-dired) (lambda (&rest _) (ert-fail "Subtree parent is already visible"))))
          (call-interactively #'dired-up-directory))
        (should (equal (dired-get-filename) child))
        (should (dired-goto-file (concat child "/space name.txt")))
        (tramp-async-dired-ops--retarget
         (list :source (list :name child :entry child :tree t)
               :target (list :name (concat root "renamed") :entry (concat root "renamed"))))
        (should (equal (dired-current-directory) (concat root "renamed/")))
        (should (dired-goto-file (concat root "renamed/space name.txt")))))))


(ert-deftest tramp-async-test-command-does-not-block-files-or-cancel-their-worker ()
  (let ((tramp-async--workers (make-hash-table :test #'equal))
        command-result file-result followup-result)
    (unwind-protect
        (let* ((dir temporary-file-directory)
               (command (tramp-async-submit
                         dir 'process-file
                         (list :argv (list (or (executable-find "sleep")
                                               (ert-fail "Missing sleep")) "10")
                               :cwd dir)
                         (lambda (result) (setq command-result result))))
               (command-worker (tramp-async-request-worker command)))
          (tramp-async-test--await (lambda () (tramp-async--worker-ready command-worker)))
          (let* ((file (tramp-async-submit
                       dir 'file-attributes (list :path dir)
                       (lambda (result) (setq file-result result))))
                 (file-worker (tramp-async-request-worker file)))
            (tramp-async-test--await (lambda () file-result))
            (should (eq (plist-get file-result :status) 'ok))
            (should-not command-result)
            (should-not (eq file-worker command-worker))
            (let ((process (tramp-async--worker-process file-worker))
                  (followup (tramp-async-call
                             dir #'sleep-for '(0.2)
                             (lambda (result) (setq followup-result result))
                             :mutating nil)))
              (tramp-async-test--await
               (lambda () (eq followup (tramp-async--worker-active file-worker))))
              (tramp-async-cancel command)
              (tramp-async-test--await (lambda () (and command-result followup-result)))
              (should (eq (plist-get command-result :status) 'cancelled))
              (should (plist-get command-result :uncertain))
              (should (eq (plist-get followup-result :status) 'ok))
              (should (eq process (tramp-async--worker-process file-worker))))))
      (tramp-async-shutdown))))

(ert-deftest tramp-async-test-queued-mutation-times-out-without-running ()
  (let ((tramp-async--workers (make-hash-table :test #'equal))
        active-result expired-result)
    (ert-with-temp-directory dir
      (unwind-protect
          (let* ((file (expand-file-name "must-not-exist" dir))
                 (active (tramp-async-call
                          dir #'sleep-for '(10)
                          (lambda (result) (setq active-result result)) :mutating nil))
                 (worker (tramp-async-request-worker active)))
            (tramp-async-test--await (lambda () (tramp-async--worker-ready worker)))
            (let ((process (tramp-async--worker-process worker)))
              (tramp-async-submit dir 'write-file (list :path file :text "too late")
                                  (lambda (result) (setq expired-result result)) :timeout 0.1)
              (tramp-async-test--await (lambda () expired-result))
              (should (eq (plist-get expired-result :status) 'timeout))
              (should-not (plist-get expired-result :uncertain))
              (should-not active-result)
              (should (eq process (tramp-async--worker-process worker)))
              (should-not (file-exists-p file))
              (should-not (tramp-async--worker-queue worker))))
        (tramp-async-shutdown)))))

(ert-deftest tramp-async-test-overdue-queue-never-starts-worker ()
  (let ((tramp-async--workers (make-hash-table :test #'equal)) result)
    (unwind-protect
        (let ((request (tramp-async-call
                        temporary-file-directory #'identity '(unexpected)
                        (lambda (reply) (setq result reply)))))
          ;; Simulate an event loop that has not delivered the timeout yet.
          (setf (tramp-async-request-deadline request) (1- (float-time)))
          (cl-letf (((symbol-function 'tramp-async--start)
                     (lambda (_) (ert-fail "Expired job started"))))
            (tramp-async--pump (tramp-async-request-worker request)))
          (tramp-async-test--await (lambda () result))
          (should (eq (plist-get result :status) 'timeout))
          (should-not (plist-get result :uncertain)))
      (tramp-async-shutdown))))

(ert-deftest tramp-async-test-file-mutations-remain-fifo ()
  (let ((tramp-async--workers (make-hash-table :test #'equal)) results)
    (ert-with-temp-directory dir
      (unwind-protect
          (let ((file (expand-file-name "ordered" dir)))
            (tramp-async-submit dir 'write-file (list :path file :text "zero")
                                (lambda (reply) (push reply results)))
            (tramp-async-submit dir 'edit-file (list :path file :old "zero" :new "one")
                                (lambda (reply) (push reply results)))
            (tramp-async-submit dir 'edit-file (list :path file :old "one" :new "two")
                                (lambda (reply) (push reply results)))
            (tramp-async-test--await (lambda () (= 3 (length results))))
            (should (seq-every-p (lambda (reply) (eq (plist-get reply :status) 'ok)) results))
            (should (equal "two" (with-temp-buffer (insert-file-contents file) (buffer-string)))))
        (tramp-async-shutdown)))))

(ert-deftest tramp-async-test-file-mutation-rejects-unsaved-buffer ()
  (let ((tramp-async--workers (make-hash-table :test #'equal)) result)
    (ert-with-temp-directory dir
      (unwind-protect
          (let ((file (expand-file-name "edited" dir)))
            (write-region "disk" nil file nil 'silent)
            (with-temp-buffer
              (setq buffer-file-name file)
              (insert "unsaved")
              (tramp-async-submit dir 'write-file (list :path file :text "overwrite")
                                  (lambda (reply) (setq result reply)))
              (tramp-async-test--await (lambda () result))
              (should (eq (plist-get result :status) 'error))
              (should (string-match-p "unsaved" (error-message-string (plist-get result :error))))
              (should (equal "unsaved" (buffer-string)))
              (setq buffer-file-name nil))
            (should (equal "disk" (with-temp-buffer (insert-file-contents file) (buffer-string)))))
        (tramp-async-shutdown)))))

(provide 'tramp-async-test)
