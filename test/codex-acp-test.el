;;; codex-acp-test.el --- Codex protocol boundary regressions -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Load with installed packages and the package directory on `load-path', then run
;; `ert-run-tests-batch-and-exit'.  These offline tests exercise raw JSON
;; framing, decoding, dispatch and encoding without contacting a model.

;;; Code:

(require 'ert)
(require 'codex-acp)

(ert-deftest codex-acp-test-loads-text-history-larger-than-four-mib ()
  "A saved conversation's aggregate tool output can exceed four MiB."
  (let* ((items (vconcat
                 (cl-loop for index below 60
                          collect `((type . "commandExecution")
                                    (id . ,(number-to-string index))
                                    (aggregatedOutput . ,(make-string 5100 ?x))))))
         (turns (make-vector 16 `((items . ,items))))
         (wire (concat (json-serialize `((id . 1) (result . ((thread . ((turns . ,turns)))))))
                       "\n{\"id\":2,\"result\":null}\n"))
         (state (codex-acp--state-create :drain 'pending)))
    (should (> (string-bytes wire) (* 4 1024 1024)))
    (unwind-protect
        (progn
          (cl-loop for start from 0 below (length wire) by 65536
                   do (codex-acp--filter state (substring wire start (min (length wire) (+ start 65536))))
                   do (should-not (codex-acp--state-closed state)))
          (let ((restored (map-nested-elt (car (codex-acp--state-messages state)) '(result thread turns))))
            (should (= 16 (length restored)))
            (should (= 60 (length (map-elt (car restored) 'items))))
            (should (= 5100 (length (map-elt (car (map-elt (car restored) 'items)) 'aggregatedOutput)))))
          (should (= 2 (map-elt (cadr (codex-acp--state-messages state)) 'id))))
      (codex-acp--close state "Test complete"))))

(ert-deftest codex-acp-test-fragmented-history-restores-multiple-large-images ()
  "A history reply may contain several attachments beyond the JSON-only limit."
  (let* ((codex-acp-json-size-limit (* 4 1024 1024))
         (acp-image-max-bytes (* 4 1024 1024))
         (png (base64-decode-string
               "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAAAF0lEQVR4nGP4z8BAEiJN9aiGUQ1DSgMAkPn/Afnh+ngAAAAASUVORK5CYII="))
         (image (acp-image-from-data
                 "image/png" (concat png (make-string (- acp-image-max-bytes (length png)) 0))))
         (url (acp-image-to-data-url image))
         (thread `((turns . [((items . [((type . "userMessage") (id . "first")
                                         (content . [((type . "image") (url . ,url))]))
                                        ((type . "userMessage") (id . "second")
                                         (content . [((type . "image") (url . ,url))]))]))])))
         (wire (concat (json-serialize `((result . ((thread . ,thread)))))
                       "\n{\"id\":2,\"result\":null}\n"))
         (state (codex-acp--state-create :drain 'pending))
         updates input)
    (unwind-protect
        (progn
          (cl-loop for start from 0 below (length wire) by 65536
                   do (codex-acp--filter state (substring wire start (min (length wire) (+ start 65536))))
                   do (should-not (codex-acp--state-closed state)))
          (setq input (codex-acp--state-input state))
          (should (= 2 (length (codex-acp--state-messages state))))
          (should (= 2 (map-elt (cadr (codex-acp--state-messages state)) 'id)))
          (cl-letf (((symbol-function 'codex-acp--update)
                     (lambda (_state update) (push update updates))))
            (codex-acp--replay-thread
             state (map-nested-elt (car (codex-acp--state-messages state)) '(result thread))))
          (should (= 2 (length updates)))
          (dolist (update updates)
            (should (equal (map-elt update 'content) image))))
      (codex-acp--close state "Test complete"))
    (should-not (buffer-live-p input))))

(ert-deftest codex-acp-test-image-prefix-can-span-small-fragments ()
  (let* ((codex-acp-json-size-limit 64)
         (acp-image-max-bytes 96)
         (wire (concat "{\"url\":\"data:image/png;base64," (make-string 128 ?A) "\"}\n"))
         (state (codex-acp--state-create :drain 'pending)))
    (unwind-protect
        (progn
          (cl-loop for start from 0 below (length wire) by 7
                   do (codex-acp--filter state (substring wire start (min (length wire) (+ start 7))))
                   do (should-not (codex-acp--state-closed state)))
          (should (= 1 (length (codex-acp--state-messages state)))))
      (codex-acp--close state "Test complete"))))

(ert-deftest codex-acp-test-framing-still-rejects-excess-json-and-images ()
  (let ((codex-acp-json-size-limit 64)
        (acp-image-max-bytes 96))
    (dolist (wire (list (concat "{\"text\":\"" (make-string 65 ?x) "\"}\n")
                       (concat "{\"url\":\"data:image/png;base64," (make-string 129 ?A) "\"}\n")
                       (concat "{\"url\":\"data:image/png;base64," (make-string 128 ?A)
                               "\",\"text\":\"" (make-string 65 ?x) "\"}\n")
                       "{}{}\n"))
      (let ((state (codex-acp--state-create :drain 'pending)))
        (unwind-protect
            (progn
              (codex-acp--filter state wire)
              (should (codex-acp--state-closed state))
              (should-not (codex-acp--state-input state)))
          (codex-acp--close state "Test complete"))))))

(ert-deftest codex-acp-test-image-limit-is-independent-of-json-budget ()
  "Reject oversized images even when the whole reply fits the JSON budget."
  (let ((codex-acp-json-size-limit 1024)
        (acp-image-max-bytes 96))
    (dolist (fragment-size '(7 1024))
      (dolist (size '(128 129))
        (let* ((wire (concat "{\"url\":\"data:image/png;base64," (make-string size ?A) "\"}\n"))
               (state (codex-acp--state-create :drain 'pending)))
          (unwind-protect
              (progn
                (cl-loop for start from 0 below (length wire) by fragment-size
                         do (codex-acp--filter state (substring wire start (min (length wire) (+ start fragment-size)))))
                (if (= size 128)
                    (progn (should-not (codex-acp--state-closed state))
                           (should (= 1 (length (codex-acp--state-messages state)))))
                  (should (codex-acp--state-closed state))))
            (codex-acp--close state "Test complete")))))))

(ert-deftest codex-acp-test-tool-output-keeps-unicode-and-escapes-invalid-bytes ()
  "Undecodable command output must still reach the model and frontend."
  (let* ((process (make-pipe-process :name "codex-acp-byte-test" :noquery t))
         (state (codex-acp--state-create :process process :thread "fixture"))
         (text (concat "café 😀 " (decode-coding-string (unibyte-string 255) 'utf-8)
                       "\nExit status: 0"))
         (expected "café 😀 \\xFF\nExit status: 0") wire update)
    (setf (codex-acp--state-client state)
          (list (cons :notification-handlers (list (lambda (event) (setq update event))))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (_process message) (setq wire message))))
            (codex-acp--tool-result state 1 text t))
          (let* ((reply (json-parse-string wire :object-type 'alist))
                 (content (map-nested-elt reply '(result contentItems))))
            (should (eq (map-nested-elt reply '(result success)) t))
            (should (equal (map-elt (aref content 0) 'text) expected)))
          (codex-acp--tool-update state '((callId . "fixture")) "completed" text)
          (let* ((content (map-nested-elt update '(params update content)))
                 (display (map-nested-elt (aref content 0) '(content text))))
            (should (equal display expected))
            (should (stringp (json-serialize update)))))
      (delete-process process))))

(ert-deftest codex-acp-test-json-null-roundtrip ()
  "Nullable response fields must agree with the outbound JSON representation."
  (let ((state (codex-acp--state-create :drain 'pending)))
    (codex-acp--filter state "{\"result\":{\"nextCursor\":nu")
    (should-not (codex-acp--state-messages state))
    (codex-acp--filter state "ll,\"name\":null,\"enabled\":false}}\n")
    (let ((result (map-elt (car (codex-acp--state-messages state)) 'result)))
      (should-not (map-elt result 'nextCursor))
      (should-not (map-elt result 'name))
      (should (eq (map-elt result 'enabled) :false))
      (should (stringp (json-serialize result :false-object :false :null-object nil))))))

(ert-deftest codex-acp-test-session-list-through-json-transport ()
  "A final real JSON page must yield usable titles and no further cursor."
  (with-temp-buffer
    (let* ((default-directory "/tmp/codex-acp-test-project/")
           (process (make-pipe-process :name "codex-acp-test" :noquery t))
           (state (codex-acp--state-create
                   :process process :initialized t :directory default-directory
                   :local-directory "/tmp/codex-acp-test-control/" :drain 'pending))
           (client (list (cons :codex-acp-state state)))
           wire result failure)
      (setf (codex-acp--state-client state) client)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'process-send-string)
                       (lambda (_process text) (setq wire text))))
              (codex-acp--request
               :client client :buffer (current-buffer)
               :request (acp-make-session-list-request :cwd default-directory)
               :on-success (lambda (value) (setq result value))
               :on-failure (lambda (value) (setq failure value))))
            (should (equal (map-elt (json-parse-string wire :object-type 'alist) 'method)
                           "thread/list"))
            (let ((params (map-elt (json-parse-string wire :object-type 'alist) 'params)))
              (should (equal (map-elt params 'cwd)
                             ["/tmp/codex-acp-test-project/" "/tmp/codex-acp-test-control/"]))
              (should (equal (map-elt params 'sourceKinds) ["cli" "vscode" "appServer"])))
            (codex-acp--filter
             state
             (concat "{\"id\":1,\"result\":{\"data\":[{\"id\":\"saved\","
                     "\"name\":null,\"preview\":\"Saved title\",\"updatedAt\":null,"
                     "\"cwd\":\"/tmp/codex-acp-test-project\"},{\"id\":\"bridge\","
                     "\"name\":\"Bridge title\",\"cwd\":\"/tmp/codex-acp-test-control\"}],"
                     "\"nextCursor\":null}}\n"))
            (codex-acp--drain state)
            (should-not failure)
            (should-not (codex-acp--state-closed state))
            (should-not (codex-acp--state-pending state))
            (should-not (map-elt result 'nextCursor))
            (should (equal (map-elt (aref (map-elt result 'sessions) 0) 'title)
                           "Saved title"))
            (should (equal (map-nested-elt (aref (map-elt result 'sessions) 0)
                                           '(_meta codex-acp-backend)) "external"))
            (should-not (map-elt (aref (map-elt result 'sessions) 1) '_meta)))
        (codex-acp--close state "Test finished")))))

(ert-deftest codex-acp-test-history-keeps-remote-workspaces-separate ()
  "Remote paths must never discover native sessions from the local host."
  (let ((state (codex-acp--state-create :directory "/ssh:host:/work/project/"
                                       :local-directory "/tmp/remote-control/")))
    (should (equal (codex-acp--session-directories state) ["/tmp/remote-control/"]))
    (should-not (codex-acp--native-session-p state '((cwd . "/work/project"))))))


(ert-deftest codex-acp-test-fork-through-json-transport ()
  "Fork config must cross the actual encoder, including dotted string keys."
  (with-temp-buffer
    (let* ((process (make-pipe-process :name "codex-acp-fork-wire-test" :noquery t))
           (state (codex-acp--state-create
                   :process process :initialized t :directory "/tmp/fork-project/"
                   :local-directory "/tmp/fork-control/" :drain 'pending))
           (client (list (cons :codex-acp-state state))) wire result failure)
      (setf (codex-acp--state-client state) client)
      (unwind-protect
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (_process text) (push text wire)))
                    ((symbol-function 'codex-acp--session-ready)
                     (lambda (_state id _model success &rest _)
                       (funcall success `((sessionId . ,id))))))
            (codex-acp--request
             :client client :buffer (current-buffer)
             :request (acp-make-session-fork-request :session-id "parent" :cwd "/tmp/fork-project/")
             :on-success (lambda (value) (setq result value))
             :on-failure (lambda (value) (setq failure value)))
            (should (equal (map-elt (json-parse-string (car wire) :object-type 'alist) 'method) "thread/read"))
            (codex-acp--filter state "{\"id\":1,\"result\":{\"thread\":{\"id\":\"parent\",\"cwd\":\"/tmp/fork-control/\"}}}\n")
            (codex-acp--drain state)
            (should-not failure)
            (let* ((request (json-parse-string (car wire) :false-object :false))
                   (params (gethash "params" request))
                   (config (gethash "config" params)))
              (should (equal (gethash "method" request) "thread/fork"))
              (should (equal (gethash "threadId" params) "parent"))
              (should (equal (gethash "cwd" params) "/tmp/fork-control/"))
              (dolist (pair codex-acp--config)
                (unless (hash-table-p (cdr pair))
                  (should (equal (gethash (car pair) config 'absent) (cdr pair)))))
              (should (hash-table-p (gethash "mcp_servers" config)))
              (should (zerop (hash-table-count (gethash "mcp_servers" config)))))
            (codex-acp--filter state "{\"id\":2,\"result\":{\"thread\":{\"id\":\"child\"},\"model\":\"fixture\"}}\n")
            (codex-acp--drain state)
            (should-not failure)
            (should (equal (map-elt result 'sessionId) "child"))
            (should-not (codex-acp--state-pending state)))
        (codex-acp--close state "Test finished")))))

(defmacro codex-acp-test-with-title (&rest body)
  "Run BODY with isolated naming state and recorded protocol requests."
  (declare (indent 0) (debug t))
  `(let* ((state (codex-acp--state-create :thread "main" :model "test-model"
                                         :directory "/tmp/title-project/"
                                         :local-directory "/tmp/title-control/"))
          (requests (make-hash-table :test 'equal))
          methods updates diagnostics)
     (setf (codex-acp--state-client state)
           (list (cons :notification-handlers (list (lambda (value) (push value updates))))
                 (cons :error-handlers (list (lambda (value) (push value diagnostics))))))
     (let ((title (codex-acp--title-state state)))
       (cl-letf (((symbol-function 'codex-acp--rpc)
                  (lambda (_state method params success failure &optional isolated)
                    ;; Exercise the native JSON boundary even with fake I/O.
                    (should (stringp (json-serialize params :null-object nil :false-object :false)))
                    (push method methods)
                    (puthash method (list params success failure isolated) requests))))
         (unwind-protect (progn ,@body)
           (setf (codex-acp--state-closed state) t)
           (codex-acp--title-stop state))))))

(defun codex-acp-test-title-reply (requests method result)
  "Reply to recorded METHOD in REQUESTS with RESULT."
  (let ((request (gethash method requests)))
    (should request)
    (remhash method requests)
    (funcall (nth 1 request) result)))

(defun codex-acp-test-title-generate (state requests)
  "Start STATE's auxiliary turn by replying to its recorded REQUESTS."
  (codex-acp-test-title-reply requests "thread/start" '((thread . ((id . "naming")))))
  (codex-acp-test-title-reply requests "turn/start" '((turn . ((id . "naming-turn")))))
  (codex-acp--notification
   state "item/completed"
   '((threadId . "naming") (turnId . "naming-turn")
     (item . ((type . "agentMessage") (text . "{\"title\":\"Brief task title\"}")))))
  (codex-acp--notification
   state "turn/completed"
   '((threadId . "naming") (turn . ((id . "naming-turn") (status . "completed"))))))

(ert-deftest codex-acp-test-title-follows-success-without-owning-main-turn ()
  "Naming is tool-free, persists once, and never completes a subsequent prompt."
  (codex-acp-test-with-title
    (let (completed)
      (codex-acp--prepare-prompt
       state '(((type . "text") (text . "@context.org:1-4\n\nExplain task: café, quotes\" and emoji 😀")))
       (lambda (_) (setq completed t)) (lambda (_) (ert-fail "Main turn failed")))
      (should-not (gethash "thread/start" requests))
      (codex-acp-test-title-reply requests "thread/inject_items" nil)
      (codex-acp-test-title-reply requests "turn/start" '((turn . ((id . "main-turn")))))
      (codex-acp--notification state "item/agentMessage/delta"
                               '((threadId . "main") (turnId . "main-turn") (delta . "Answer")))
      (codex-acp--notification state "turn/completed"
                               '((threadId . "main") (turn . ((id . "main-turn") (status . "completed")))))
      (should completed)
      (let* ((request (gethash "thread/start" requests)) (params (car request)))
        (should (nth 3 request))
        (should (eq (map-elt params 'ephemeral) t))
        (should (equal (map-elt params 'environments) []))
        (should (equal (map-elt params 'dynamicTools) []))
        (should (equal (map-elt params 'approvalPolicy) "never")))
      (should (equal (codex-acp--title-response title) "Answer"))
      (setf (codex-acp--state-prompt state) 'next-prompt)
      (codex-acp-test-title-generate state requests)
      (should (eq (codex-acp--state-prompt state) 'next-prompt))
      (should (= (length updates) 1))
      (codex-acp-test-title-reply requests "thread/read" '((thread . ((name . nil)))))
      (should (equal (map-elt (car (gethash "thread/name/set" requests)) 'threadId) "main"))
      (codex-acp-test-title-reply requests "thread/name/set" nil)
      (should (equal (codex-acp--title-name title) "Brief task title"))
      (should (equal (map-nested-elt (car updates) '(params update sessionUpdate)) "session_info_update"))
      (should-not (codex-acp--title-timer title))
      (should-not diagnostics)
      (codex-acp--title-start state)
      (should (= (cl-count "thread/start" methods :test #'equal) 1)))))

(ert-deftest codex-acp-test-title-preserves-existing-and-concurrent-names ()
  "Known names and names discovered immediately before saving take precedence."
  (codex-acp-test-with-title
    (codex-acp--title-ready state '((name . "Existing name") (preview . "long preview")))
    (should-not methods)
    (should (equal (codex-acp--title-name title) "Existing name")))
  (codex-acp-test-with-title
    (codex-acp--title-ready state '((name . nil) (preview . "old unnamed task")))
    (should (equal (codex-acp--title-input title) "old unnamed task"))
    (codex-acp-test-title-generate state requests)
    (codex-acp-test-title-reply requests "thread/read" '((thread . ((name . "Manual rename")))))
    (should-not (gethash "thread/name/set" requests))
    (should (equal (codex-acp--title-name title) "Manual rename")))
  (codex-acp-test-with-title
    (codex-acp--title-ready state '((preview . "unnamed task")))
    (codex-acp-test-title-reply requests "thread/start" '((thread . ((id . "naming")))))
    (codex-acp-test-title-reply requests "turn/start" '((turn . ((id . "naming-turn")))))
    (codex-acp--notification state "thread/name/updated"
                             '((threadId . "main") (threadName . "Renamed while generating")))
    (should (gethash "turn/interrupt" requests))
    (should (eq (codex-acp--title-phase title) 'done))
    (codex-acp--notification state "turn/completed"
                             '((threadId . "naming") (turn . ((id . "naming-turn") (status . "completed")))))
    (should-not (gethash "thread/name/set" requests))
    (should (equal (codex-acp--title-name title) "Renamed while generating"))))

(ert-deftest codex-acp-test-title-invalid-output-is-isolated ()
  "Malformed, empty, multiline and excessively large outputs cannot fail a chat."
  (dolist (output '("not JSON" "true" "{\"title\":\" \"}" "{\"title\":\"one\\ntwo\"}"))
    (codex-acp-test-with-title
      (codex-acp--title-ready state '((preview . "unnamed task")))
      (codex-acp-test-title-reply requests "thread/start" '((thread . ((id . "naming")))))
      (codex-acp-test-title-reply requests "turn/start" '((turn . ((id . "naming-turn")))))
      (setf (codex-acp--state-prompt state) 'active
            (codex-acp--title-output title) output)
      (codex-acp--notification state "turn/completed"
                               '((threadId . "naming") (turn . ((id . "naming-turn") (status . "completed")))))
      (should diagnostics)
      (should-not (codex-acp--state-closed state))
      (should (eq (codex-acp--state-prompt state) 'active))
      (should-not (gethash "thread/name/set" requests))))
  (codex-acp-test-with-title
    (codex-acp--title-ready state '((preview . "unnamed task")))
    (codex-acp-test-title-reply requests "thread/start" '((thread . ((id . "naming")))))
    (codex-acp--notification state "item/agentMessage/delta"
                             `((threadId . "naming") (delta . ,(make-string 2049 ?x))))
    (should diagnostics)
    (should (eq (codex-acp--title-phase title) 'done))))

(ert-deftest codex-acp-test-title-timeout-and-close-release-resources ()
  "Timeout cleans up only naming; disconnect cancels its timers quietly."
  (codex-acp-test-with-title
    (codex-acp--title-ready state '((preview . "unnamed task")))
    (codex-acp-test-title-reply requests "thread/start" '((thread . ((id . "naming")))))
    (codex-acp-test-title-reply requests "turn/start" '((turn . ((id . "naming-turn")))))
    (codex-acp--title-stop state '((message . "Title generation timed out")))
    (should-not (codex-acp--state-closed state))
    (should-not (codex-acp--title-timer title))
    (should-not (codex-acp--title-thread title))
    (codex-acp-test-title-reply requests "turn/interrupt" nil)
    (should (gethash "thread/unsubscribe" requests))
    (should (= (length diagnostics) 1)))
  (codex-acp-test-with-title
    (codex-acp--title-ready state '((preview . "unnamed task")))
    (codex-acp--close state "Test disconnect")
    (should-not (codex-acp--title-timer title))
    (should (eq (codex-acp--title-phase title) 'done))
    (should-not diagnostics)))

(ert-deftest codex-acp-test-title-control-timeout-does-not-close-transport ()
  "An auxiliary start timeout must not close the shared main transport."
  (let* ((process (make-pipe-process :name "codex-title-timeout-test" :noquery t))
         (state (codex-acp--state-create :process process))
         (codex-acp-request-timeout 0.01)
         failed)
    (unwind-protect
        (cl-letf (((symbol-function 'process-send-string) #'ignore))
          (codex-acp--rpc state "thread/start" nil #'ignore (lambda (_) (setq failed t)) t)
          (let ((deadline (+ (float-time) 1)))
            (while (and (not failed) (< (float-time) deadline))
              (accept-process-output nil 0.02)))
          (should failed)
          (should-not (codex-acp--state-closed state))
          (should (process-live-p process)))
      (codex-acp--close state "Test finished"))))

(ert-deftest codex-acp-test-title-generation-deadline-preserves-main-prompt ()
  "The actual naming deadline must report once and leave a running chat intact."
  (codex-acp-test-with-title
    (let ((codex-acp-title-timeout 0.01))
      (setf (codex-acp--state-prompt state) 'main-prompt)
      (codex-acp--title-ready state '((preview . "task")))
      (let ((deadline (+ (float-time) 1)))
        (while (and (not diagnostics) (< (float-time) deadline))
          (accept-process-output nil 0.02)))
      (should (= (length diagnostics) 1))
      (should (eq (codex-acp--title-phase title) 'done))
      (should-not (codex-acp--title-timer title))
      (should-not (codex-acp--state-closed state))
      (should (eq (codex-acp--state-prompt state) 'main-prompt)))))

(ert-deftest codex-acp-test-title-late-starts-are-released ()
  "Late thread and turn acknowledgements cannot resurrect cancelled naming."
  (codex-acp-test-with-title
    (codex-acp--title-ready state '((preview . "task")))
    (codex-acp--title-stop state '((message . "Timed out")))
    (codex-acp-test-title-reply requests "thread/start" '((thread . ((id . "late-thread")))))
    (should (equal (map-elt (car (gethash "thread/unsubscribe" requests)) 'threadId) "late-thread"))
    (should-not (gethash "turn/start" requests))
    (should (eq (codex-acp--title-phase title) 'done)))
  (codex-acp-test-with-title
    (codex-acp--title-ready state '((preview . "task")))
    (codex-acp-test-title-reply requests "thread/start" '((thread . ((id . "naming")))))
    (codex-acp--title-stop state '((message . "Timed out")))
    (codex-acp-test-title-reply requests "turn/start" '((turn . ((id . "late-turn")))))
    (should (equal (map-elt (car (gethash "turn/interrupt" requests)) 'turnId) "late-turn"))
    (should (eq (codex-acp--title-phase title) 'done))))

(ert-deftest codex-acp-test-title-skips-failed-turns-and-bounds-source ()
  "Failure or cancellation cannot trigger naming; large context keeps its tail."
  (let ((excerpt (codex-acp--title-excerpt (concat "begin" (make-string 10000 ?x) "actual task") 8000)))
    (should (<= (length excerpt) 8000))
    (should (string-prefix-p "begin" excerpt))
    (should (string-suffix-p "actual task" excerpt)))
  (dolist (cancelled '(nil t))
    (codex-acp-test-with-title
      (setf (codex-acp--title-input title) "task"
            (codex-acp--state-prompt state) (cons #'ignore #'ignore))
      (codex-acp--finish state (unless cancelled '((message . "Failed"))) cancelled)
      (should-not methods)
      (should-not (codex-acp--title-attempted title)))))

(defconst codex-acp-test-effort-models
  [((model . "deep") (displayName . "Deep") (defaultReasoningEffort . "medium")
    (supportedReasoningEfforts . [((reasoningEffort . "medium")) ((reasoningEffort . "ultra"))]))
   ((model . "fast") (displayName . "Fast") (defaultReasoningEffort . "low")
    (supportedReasoningEfforts . [((reasoningEffort . "low")) ((reasoningEffort . "medium"))]))
   ((model . "plain") (displayName . "Plain") (supportedReasoningEfforts . []))])

(ert-deftest codex-acp-test-effort-settings-and-subsequent-turns ()
  "Only advertised values may change settings, and every turn carries effort."
  (let* ((state (codex-acp--state-create :thread "main" :model "deep"
                                       :directory "/tmp/effort-project/"
                                       :models codex-acp-test-effort-models))
         (client (list (cons :codex-acp-state state)))
         result failure wire)
    (setf (codex-acp--state-client state) client)
    (cl-labels ((select (id value &optional session-id)
                  (setq result nil failure nil)
                  (codex-acp--request
                   :client client
                   :request (acp-make-session-set-config-option-request
                             :session-id (or session-id "main") :config-id id :value value)
                   :on-success (lambda (value) (setq result value))
                   :on-failure (lambda (value) (setq failure value)))))
      (select "model" "deep")
      (should-not failure)
      (should (equal (map-elt (aref (map-elt result 'configOptions) 1) 'currentValue) "medium"))
      (select "effort" "ultra")
      (should-not failure)
      (should (equal (map-elt (codex-acp--state-client state) :codex-acp-effort) "ultra"))
      (select "effort" "low")
      (should failure)
      (select "effort" "medium" "other-session")
      (should failure)
      (setf (codex-acp--state-prompt state) t)
      (select "effort" "medium")
      (should failure)
      (cl-letf (((symbol-function 'codex-acp--rpc)
                 (lambda (_state method params success &rest _)
                   (if (equal method "thread/inject_items") (funcall success nil)
                     (should (equal method "turn/start"))
                     (push (json-parse-string (json-serialize params :null-object nil)
                                             :object-type 'alist) wire)))))
        (dotimes (_ 2) (codex-acp--start-turn state '(((type . "text") (text . "test"))))))
      (should (= (length wire) 2))
      (dolist (params wire)
        (should (equal (map-elt params 'model) "deep"))
        (should (equal (map-elt params 'effort) "ultra")))
      (setf (codex-acp--state-prompt state) nil)
      (select "model" "fast")
      (should-not failure)
      (should (equal (map-elt (codex-acp--state-client state) :codex-acp-effort) "low"))
      (should (equal (mapcar (lambda (item) (map-elt item 'value))
                            (append (map-elt (aref (map-elt result 'configOptions) 1) 'options) nil))
                     '("low" "medium")))
      (select "effort" "medium")
      (select "model" "deep")
      (should (equal (map-elt (codex-acp--state-client state) :codex-acp-effort) "medium"))
      (select "model" "plain")
      (should (= (length (map-elt result 'configOptions)) 1))
      (should-not (map-elt (codex-acp--state-client state) :codex-acp-effort))
      (select "effort" "medium")
      (should failure))))

(ert-deftest codex-acp-test-effort-session-ready-and-legacy-change ()
  "Advertise the restored effort, and refresh options for legacy model changes."
  (let* ((state (codex-acp--state-create))
         (client (list (cons :codex-acp-state state)))
         result update failure)
    (setf (codex-acp--state-client state) client)
    (cl-letf (((symbol-function 'codex-acp--rpc)
               (lambda (_state method _params success &rest _)
                 (should (equal method "model/list"))
                 (funcall success `((data . ,codex-acp-test-effort-models)))))
              ((symbol-function 'codex-acp--title-ready) #'ignore)
              ((symbol-function 'codex-acp--refresh-skills) #'ignore)
              ((symbol-function 'codex-acp--update)
               (lambda (_state value) (setq update value))))
      (codex-acp--session-ready state "main" "deep" (lambda (value) (setq result value)) nil "ultra")
      (should (equal (map-elt (aref (map-elt result 'configOptions) 1) 'currentValue) "ultra"))
      (codex-acp--request
       :client client :request (acp-make-session-set-model-request :session-id "main" :model-id "fast")
       :on-failure (lambda (value) (setq failure value)))
      (should-not failure)
      (should (equal (map-elt update 'sessionUpdate) "config_option_update"))
      (should (equal (map-elt (aref (map-elt update 'configOptions) 1) 'currentValue) "low")))))

(provide 'codex-acp-test)
;;; codex-acp-test.el ends here
