;;; treemacs-workflows.el --- Warp-like workflows inside Treemacs -*- lexical-binding: t; -*-

;; Author: Jungmo Ahn
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (treemacs "0") (vterm "0") (multi-vterm "0"))
;; Keywords: convenience, tools

;;; Commentary:
;;
;; Warp-style workflows integrated into Treemacs.
;;
;; Features:
;; - Adds a "Workflows" section under each Treemacs project.
;; - RET on a workflow runs it in the project's dedicated vterm (session 1).
;; - Supports global workflows (custom variable `enotion/workflows`) and per-project
;;   overrides in a data-only file (default: .workflows.el) stored in the project
;;   root. The per-project file is READ (not eval'd).
;; - Workflow spec supports:
;;     - "cmd" string
;;     - plist: (:cmd "..." :confirm t :cwd "subdir" :env (("K" . "V") ...))
;; - Treemacs keys:
;;     a  add to local  (per-project) .workflows.el
;;     A  add to global enotion/workflows
;;     d  delete (local if exists, else global)
;;     r  rename (local if exists, else global)
;;     e  open/edit local .workflows.el
;;     E  edit global enotion/workflows
;;
;; Install:
;; 1) Put this file in ~/.emacs.d/lisp/treemacs-workflows.el
;; 2) In init.el:
;;      (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;;      (require 'treemacs-workflows)
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'project)
(require 'treemacs)
(require 'treemacs-treelib)

(require 'vterm)
(require 'multi-vterm)

;;;; ============================================================
;;;; Project vterm (A default: session 1 per project)
;;;; ============================================================

(defun enotion/wf--project-root ()
  "Return current project root, or `default-directory`."
  (or (when-let ((pr (project-current nil)))
        (expand-file-name (project-root pr)))
      (expand-file-name default-directory)))

(defun enotion/wf--project-name (root)
  (file-name-nondirectory (directory-file-name (expand-file-name root))))

(defun enotion/wf--vterm-bufname (root &optional n)
  (let* ((n (max 1 (or n 1)))
         (p (enotion/wf--project-name root)))
    (if (= n 1)
        (format "*vterm:proj:%s*" p)
      (format "*vterm:proj:%s#%d*" p n))))

(defun enotion/wf--get-vterm-for-root (root &optional n)
  "Get or create vterm buffer for ROOT project and session N (default 1)."
  (let* ((root (expand-file-name root))
         (bufname (enotion/wf--vterm-bufname root n))
         (buf (get-buffer bufname)))
    (unless (buffer-live-p buf)
      (let ((default-directory root))
        (setq buf (multi-vterm))
        (with-current-buffer buf
          (rename-buffer bufname t)
          (setq default-directory root))))
    buf))

(defun enotion/wf--send-in-root (root cmd &optional n)
  "Run CMD in ROOT project's vterm session N (default 1)."
  (let ((buf (enotion/wf--get-vterm-for-root root (or n 1))))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (vterm-send-string cmd)
      (vterm-send-return))))

(defun enotion/wf-open-project-vterm ()
  "Open (or create) the current project's vterm (session 1)."
  (interactive)
  (let* ((root (enotion/wf--project-root))
         (buf (enotion/wf--get-vterm-for-root root 1)))
    (pop-to-buffer buf)))

;;;; ============================================================
;;;; Workflows: global + local (.workflows.el)
;;;; ============================================================

(defgroup enotion-workflows nil
  "Warp-like workflows in Treemacs."
  :group 'convenience)

(defcustom enotion/workflows-local-file ".workflows.el"
  "Per-project workflow overlay file name under project root."
  :type 'string
  :group 'enotion-workflows)

(defcustom enotion/workflows
  '(("Starter"
     ("Example workflow" . "echo hello-from-workflow")
     ("List files"       . "ls -la"))
    ("Git"
     ("Status"           . "git status")
     ("Fetch"            . "git fetch --all --prune")))
  "Global workflows.

Format: ((GROUP (LABEL . SPEC) ...) ...)

SPEC can be:
- string: \"command\"
- plist: (:cmd \"...\" :confirm t :cwd \"subdir\" :env ((\"K\" . \"V\") ...))"
  :type '(repeat (list (string :tag "Group")
                       (repeat (cons (string :tag "Label")
                                    (sexp :tag "Spec")))))
  :group 'enotion-workflows)

(defcustom enotion/workflows-global-file (expand-file-name "~/org/workflows.el")
  "Global workflows data file.

If present, its first sexp (data only) replaces `enotion/workflows` at load time."
  :type 'file
  :group 'enotion-workflows)

(defun enotion/wf--save-global ()
  (enotion/wf--write-sexp-file enotion/workflows-global-file enotion/workflows))

(defvar enotion/wf--local-cache (make-hash-table :test 'equal)
  "Cache of local workflows: root -> (mtime . sexp).")

(defun enotion/wf--local-file (root)
  (expand-file-name enotion/workflows-local-file (expand-file-name root)))

(defun enotion/wf--read-sexp-file (file)
  "Read first sexp from FILE without eval. Return nil on error."
  (when (file-readable-p file)
    (condition-case _
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((data (read (current-buffer))))
            (when (listp data) data)))
      (error nil))))

(defun enotion/wf--write-sexp-file (file sexp)
  "Write SEXP as a single sexp to FILE (pretty-printed)."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (let ((print-level nil)
          (print-length nil))
      (insert ";; Project workflows (data only). No eval.\n")
      (insert ";; Format: ((GROUP (LABEL . SPEC) ...) ...)\n\n")
      (pp sexp (current-buffer)))))

(defun enotion/wf--load-global ()
  "Load global workflows from `enotion/workflows-global-file` if present."
  (let ((data (enotion/wf--read-sexp-file enotion/workflows-global-file)))
    (when (listp data)
      (setq enotion/workflows data))))

(enotion/wf--load-global)

(defun enotion/wf--local-get (root)
  "Get cached local workflows for ROOT."
  (let* ((root (expand-file-name root))
         (file (enotion/wf--local-file root)))
    (if (not (file-exists-p file))
        (progn (remhash root enotion/wf--local-cache) nil)
      (let* ((mtime (file-attribute-modification-time (file-attributes file)))
             (cached (gethash root enotion/wf--local-cache)))
        (if (and cached (equal (car cached) mtime))
            (cdr cached)
          (let ((data (enotion/wf--read-sexp-file file)))
            (puthash root (cons mtime data) enotion/wf--local-cache)
            data))))))

(defun enotion/wf--local-set (root data)
  "Write DATA to ROOT local file and update cache."
  (let* ((root (expand-file-name root))
         (file (enotion/wf--local-file root)))
    (enotion/wf--write-sexp-file file data)
    (let ((mtime (file-attribute-modification-time (file-attributes file))))
      (puthash root (cons mtime data) enotion/wf--local-cache))
    data))

(defun enotion/wf--merge (base overlay)
  "Merge OVERLAY into BASE. Same group/label overrides; new appended."
  (let ((result (copy-tree base)))
    (dolist (g overlay)
      (let* ((gname (car g))
             (items (cdr g))
             (rg (assoc gname result)))
        (if (not rg)
            (setq result (append result (list (cons gname (copy-tree items)))))
          (dolist (it items)
            (let* ((label (car it))
                   (cell (assoc label (cdr rg))))
              (if cell
                  (setcdr cell (cdr it))
                (setcdr rg (append (cdr rg) (list (cons label (cdr it)))))))))))
    result))

(defun enotion/wf--workflows-for-root (root)
  (let ((local (enotion/wf--local-get root)))
    (if (and local (listp local))
        (enotion/wf--merge enotion/workflows local)
      enotion/workflows)))

(defun enotion/wf--groups (root)
  (mapcar #'car (enotion/wf--workflows-for-root root)))

(defun enotion/wf--items (root group)
  (cdr (assoc group (enotion/wf--workflows-for-root root))))

(defun enotion/wf--labels (root group)
  (mapcar #'car (enotion/wf--items root group)))

(defun enotion/wf--spec (root group label)
  (cdr (assoc label (enotion/wf--items root group))))

(defun enotion/wf--spec->plist (spec)
  (cond
   ((stringp spec) (list :cmd spec))
   ((and (listp spec) (plist-get spec :cmd)) spec)
   (t (list :cmd (format "%s" spec)))))

(defun enotion/wf--env-prefix (env)
  (when (and env (listp env))
    (mapconcat (lambda (kv)
                 (format "%s=%s" (car kv) (shell-quote-argument (cdr kv))))
               env " ")))

(defun enotion/wf--compose-shell (root plist)
  (let* ((cmd (plist-get plist :cmd))
         (cwd (plist-get plist :cwd))
         (env (plist-get plist :env))
         (envp (enotion/wf--env-prefix env))
         (dir (if (and cwd (stringp cwd) (not (string-empty-p cwd)))
                  (expand-file-name cwd root)
                root))
         (cdpart (format "cd %s" (shell-quote-argument dir)))
         (body (if envp (format "%s %s" envp cmd) cmd)))
    (format "%s && %s" cdpart body)))

(defun enotion/wf--maybe-confirm (plist display)
  (let ((confirm (plist-get plist :confirm)))
    (or (not confirm)
        (y-or-n-p (format "Run workflow '%s'? " display)))))

;;;; ============================================================
;;;; Status tracking (never/running/success/fail + last run age)
;;;; ============================================================

(defcustom enotion/wf-status-glyph-never "○"
  "Glyph shown for workflows that were never run."
  :type 'string
  :group 'enotion-workflows)

(defcustom enotion/wf-status-glyph-running "⏳"
  "Glyph shown while a workflow is running."
  :type 'string
  :group 'enotion-workflows)

(defcustom enotion/wf-status-glyph-success "✅"
  "Glyph shown when last run succeeded (exit 0)."
  :type 'string
  :group 'enotion-workflows)

(defcustom enotion/wf-status-glyph-fail "❌"
  "Glyph shown when last run failed (exit non-zero)."
  :type 'string
  :group 'enotion-workflows)

(defcustom enotion/wf-status-show-last-run t
  "Whether to append a short last-run age to the workflow label in Treemacs."
  :type 'boolean
  :group 'enotion-workflows)

(defcustom enotion/wf-status-last-run-max-age-seconds (* 60 60 24 7)
  "If last run is older than this, hide the age suffix."
  :type 'integer
  :group 'enotion-workflows)

(defvar enotion/wf--status (make-hash-table :test 'equal)
  "Workflow status table.

Key: (ROOT GROUP LABEL)
Val: plist (:state never|running|success|fail :time FLOAT :exit INT).")

(defvar enotion/wf--token->ctx (make-hash-table :test 'equal)
  "Run token -> plist (:root :group :label :buf).")

(defconst enotion/wf--marker-prefix "__WF_STATUS__"
  "Marker prefix printed by workflows to communicate exit status.")

(defun enotion/wf--status-key (root group label)
  (list (expand-file-name root) group label))

(defun enotion/wf--status-get (root group label)
  (gethash (enotion/wf--status-key root group label) enotion/wf--status))

(defun enotion/wf--status-set (root group label plist)
  (puthash (enotion/wf--status-key root group label) plist enotion/wf--status))

(defun enotion/wf--status-glyph (state exit)
  (pcase state
    ('running enotion/wf-status-glyph-running)
    ('success enotion/wf-status-glyph-success)
    ('fail enotion/wf-status-glyph-fail)
    (_ enotion/wf-status-glyph-never)))

(defun enotion/wf--human-age (seconds)
  "Return a short human string like 3m/2h/5d."
  (cond
   ((< seconds 60) (format "%ds" (floor seconds)))
   ((< seconds (* 60 60)) (format "%dm" (floor (/ seconds 60.0))))
   ((< seconds (* 60 60 24)) (format "%dh" (floor (/ seconds 3600.0))))
   (t (format "%dd" (floor (/ seconds 86400.0))))))

(defun enotion/wf--decorate-label (root group label)
  "Return Treemacs label with status glyph (+ optional age)."
  (let* ((st (enotion/wf--status-get root group label))
         (state (plist-get st :state))
         (exit (plist-get st :exit))
         (t0 (plist-get st :time))
         (glyph (enotion/wf--status-glyph state exit))
         (age (when (and enotion/wf-status-show-last-run t0)
                (let ((dt (- (float-time) t0)))
                  (when (< dt enotion/wf-status-last-run-max-age-seconds)
                    (enotion/wf--human-age dt))))))
    (if age
        (format "%s %s  (%s)" glyph label age)
      (format "%s %s" glyph label))))

(defun enotion/wf--make-token ()
  (substring (md5 (format "%s-%s-%s" (float-time) (emacs-pid) (random))) 0 10))

(defun enotion/wf--wrap-cmd-with-marker (root plist token)
  "Build final shell command that prints a marker with numeric exit status."
  (let* ((base (enotion/wf--compose-shell root plist)))
    ;; IMPORTANT: use double quotes so $__wf_ec expands.
    (format "( %s ); __wf_ec=$?; echo \"%s:%s:$__wf_ec\"" base enotion/wf--marker-prefix token)))

(defun enotion/wf--watch-token (token)
  "Poll vterm buffer for TOKEN marker; update status when found."
  (let* ((ctx (gethash token enotion/wf--token->ctx))
         (buf (plist-get ctx :buf))
         (root (plist-get ctx :root))
         (group (plist-get ctx :group))
         (label (plist-get ctx :label)))
    (when (and ctx (buffer-live-p buf))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-max))
          (let* ((needle (format "%s:%s:" enotion/wf--marker-prefix token))
                 (found (search-backward needle nil t)))
            (when found
              (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                                           (line-end-position)))
                     (exit (when (string-match (format "%s:%s:\\([0-9]+\\)" enotion/wf--marker-prefix token)
                                               line)
                             (string-to-number (match-string 1 line))))
                     (state (if (and (numberp exit) (= exit 0)) 'success 'fail)))
                (enotion/wf--status-set root group label
                                   (list :state state :time (float-time) :exit exit))
                (remhash token enotion/wf--token->ctx)
                (enotion/wf--treemacs-refresh)
                t))))))))

(defun enotion/wf--start-watch (token)
  "Start a timer that watches for TOKEN completion for up to ~10 minutes."
  (let* ((start (float-time))
         (timer nil))
    (setq timer
          (run-with-timer
           0.3 0.3
           (lambda ()
             (cond
              ((enotion/wf--watch-token token)
               (when (timerp timer) (cancel-timer timer)))
              ((> (- (float-time) start) (* 60 10))
               ;; timeout: mark fail
               (let ((ctx (gethash token enotion/wf--token->ctx)))
                 (when ctx
                   (enotion/wf--status-set (plist-get ctx :root)
                                      (plist-get ctx :group)
                                      (plist-get ctx :label)
                                      (list :state 'fail :time (float-time) :exit nil))
                   (remhash token enotion/wf--token->ctx)
                   (enotion/wf--treemacs-refresh)))
               (when (timerp timer) (cancel-timer timer)))))))
    timer))

(defun enotion/wf-run-for-root (root group label)
  "Run workflow GROUP/LABEL for project ROOT in vterm session 1.

Updates Treemacs label with running/success/fail based on an echoed marker."
  (let* ((spec (enotion/wf--spec root group label))
         (plist (enotion/wf--spec->plist spec))
         (display (format "%s / %s" group label)))
    (when (enotion/wf--maybe-confirm plist display)
      (let* ((token (enotion/wf--make-token))
             (buf (enotion/wf--get-vterm-for-root root 1))
             (cmd (enotion/wf--wrap-cmd-with-marker root plist token)))
        (enotion/wf--status-set root group label (list :state 'running :time (float-time) :exit nil))
        (puthash token (list :root (expand-file-name root)
                             :group group :label label :buf buf)
                 enotion/wf--token->ctx)
        (enotion/wf--treemacs-refresh)
        (enotion/wf--send-in-root root cmd 1)
        (enotion/wf--start-watch token)))))

;;;; ============================================================
;;;; Treemacs integration
;;;; ============================================================

(defun enotion/wf--treemacs-refresh ()
  (when (fboundp 'treemacs-get-local-window)
    (when-let ((win (treemacs-get-local-window)))
      (with-selected-window win
        (treemacs-refresh)))))

(defun enotion/wf--ctx ()
  "Treemacs node context: plist :root :group :label"
  (let* ((btn (ignore-errors (treemacs-current-button)))
         (root (and btn (treemacs-safe-button-get btn :wf-root)))
         (group (and btn (treemacs-safe-button-get btn :wf-group)))
         (label (and btn (treemacs-safe-button-get btn :wf-label))))
    (list :root root :group group :label label :btn btn)))

(defun enotion/wf-treemacs-RET (&optional _)
  (interactive)
  (let* ((ctx (enotion/wf--ctx))
         (root (plist-get ctx :root))
         (group (plist-get ctx :group))
         (label (plist-get ctx :label)))
    (when (and root group label)
      (enotion/wf-run-for-root root group label))))

(treemacs-define-entry-node-type enotion-workflows
  :label "Workflows"
  :key "enotion-workflows-root"
  :open-icon (treemacs-get-icon-value 'root-open)
  :closed-icon (treemacs-get-icon-value 'root-closed)
  :children (enotion/wf--groups (car (treemacs-button-get btn :path)))
  :child-type 'enotion-workflows-group)

(treemacs-define-expandable-node-type enotion-workflows-group
  :label item
  :key item
  :open-icon (treemacs-get-icon-value 'tag-open)
  :closed-icon (treemacs-get-icon-value 'tag-closed)
  :children (enotion/wf--labels (car (treemacs-button-get btn :path)) item)
  :child-type 'enotion-workflows-item
  :more-properties (list :wf-root (car (treemacs-button-get btn :path))
                         :wf-group item))

(treemacs-define-leaf-node-type enotion-workflows-item
  :label (enotion/wf--decorate-label (car (treemacs-button-get btn :path))
                                (treemacs-button-get btn :wf-group)
                                item)
  :key item
  :icon (treemacs-get-icon-value 'terminal)
  :ret-action #'enotion/wf-treemacs-RET
  :visit-action (lambda (_btn) (enotion/wf-treemacs-RET))
  :more-properties (list :wf-root (car (treemacs-button-get btn :path))
                         :wf-group (treemacs-button-get btn :wf-group)
                         :wf-label item))

(treemacs-enable-project-extension :extension 'enotion-workflows :position 'top)

;;;; ============================================================
;;;; Treemacs keys: a/A/d/r/e (local-first)
;;;; ============================================================

(defun enotion/wf--parse-env (envstr)
  "Parse KEY=VAL,KEY2=VAL2 -> alist."
  (when (and envstr (not (string-empty-p envstr)))
    (mapcar (lambda (pair)
              (let* ((kv (split-string pair "=" t))
                     (k (car kv))
                     (v (string-join (cdr kv) "=")))
                (cons k v)))
            (split-string envstr "," t "[[:space:]]+"))))

(defun enotion/wf--prompt-spec ()
  "Prompt for workflow spec and return plist."
  (let* ((cmd (read-string "Shell cmd: "))
         (confirm (y-or-n-p "Confirm before run? "))
         (cwd (read-string "CWD (subdir, empty=project root): "))
         (envstr (read-string "Env (KEY=VAL,KEY2=VAL2 empty=none): "))
         (env (enotion/wf--parse-env envstr)))
    (append (list :cmd cmd)
            (when confirm (list :confirm t))
            (when (not (string-empty-p cwd)) (list :cwd cwd))
            (when env (list :env env)))))

;; ---- global mutate helpers ----
(defun enotion/wf--global-add (group label spec)
  (let ((g (assoc group enotion/workflows)))
    (unless g
      (setq enotion/workflows (append enotion/workflows (list (cons group nil))))
      (setq g (assoc group enotion/workflows)))
    (let ((cell (assoc label (cdr g))))
      (if cell
          (setcdr cell spec)
        (setcdr g (append (cdr g) (list (cons label spec))))))))

(defun enotion/wf--global-del (group label)
  (let ((g (assoc group enotion/workflows)))
    (when g
      (setcdr g (cl-remove-if (lambda (it) (string= (car it) label)) (cdr g))))))

(defun enotion/wf--global-get (group label)
  (cdr (assoc label (cdr (assoc group enotion/workflows)))))

;; ---- local mutate helpers ----
(defun enotion/wf--local-add (root group label spec)
  (let* ((local (or (enotion/wf--local-get root) '()))
         (g (assoc group local)))
    (unless g
      (setq local (append local (list (cons group nil))))
      (setq g (assoc group local)))
    (let ((cell (assoc label (cdr g))))
      (if cell
          (setcdr cell spec)
        (setcdr g (append (cdr g) (list (cons label spec))))))
    (enotion/wf--local-set root local)))

(defun enotion/wf--local-del (root group label)
  (let* ((local (or (enotion/wf--local-get root) '()))
         (g (assoc group local)))
    (when g
      (setcdr g (cl-remove-if (lambda (it) (string= (car it) label)) (cdr g))))
    (enotion/wf--local-set root local)))

(defun enotion/wf--local-get1 (root group label)
  (let* ((local (enotion/wf--local-get root))
         (g (and local (assoc group local))))
    (and g (cdr (assoc label (cdr g))))))

(defun enotion/wf--has-local-p (root group label)
  (and (enotion/wf--local-get1 root group label) t))

(defun enotion/wf-treemacs-add-local ()
  "Treemacs: a (add workflow to local .workflows.el for that project)."
  (interactive)
  (let* ((ctx (enotion/wf--ctx))
         (root (or (plist-get ctx :root) (enotion/wf--project-root)))
         (group0 (or (plist-get ctx :group) "Starter"))
         (group (read-string "Group: " group0))
         (label (read-string "Label: "))
         (spec (enotion/wf--prompt-spec)))
    (enotion/wf--local-add root group label spec)
    (enotion/wf--treemacs-refresh)
    (message "Added (local): %s / %s" group label)))

(defun enotion/wf-treemacs-add-global ()
  "Treemacs: A (add workflow to global enotion/workflows)."
  (interactive)
  (let* ((ctx (enotion/wf--ctx))
         (group0 (or (plist-get ctx :group) "Starter"))
         (group (read-string "Group: " group0))
         (label (read-string "Label: "))
         (spec (enotion/wf--prompt-spec)))
    (enotion/wf--global-add group label spec)
    (enotion/wf--save-global)
    (enotion/wf--treemacs-refresh)
    (message "Added (global): %s / %s" group label)))

(defun enotion/wf-treemacs-delete ()
  "Treemacs: d (delete local if exists else delete global)."
  (interactive)
  (let* ((ctx (enotion/wf--ctx))
         (root (plist-get ctx :root))
         (group (plist-get ctx :group))
         (label (plist-get ctx :label)))
    (unless (and root group label)
      (user-error "Delete: workflow item 위에서 실행하세요"))
    (if (enotion/wf--has-local-p root group label)
        (when (y-or-n-p (format "Delete LOCAL '%s / %s'? " group label))
          (enotion/wf--local-del root group label)
          (enotion/wf--treemacs-refresh)
          (message "Deleted (local): %s / %s" group label))
      (when (y-or-n-p (format "Delete GLOBAL '%s / %s'? " group label))
        (enotion/wf--global-del group label)
        (enotion/wf--save-global)
        (enotion/wf--treemacs-refresh)
        (message "Deleted (global): %s / %s" group label)))))

(defun enotion/wf-treemacs-rename ()
  "Treemacs: r (rename local if exists else rename global)."
  (interactive)
  (let* ((ctx (enotion/wf--ctx))
         (root (plist-get ctx :root))
         (group (plist-get ctx :group))
         (label (plist-get ctx :label)))
    (unless (and root group label)
      (user-error "Rename: workflow item 위에서 실행하세요"))
    (let ((new (read-string "New label: " label)))
      (if (enotion/wf--has-local-p root group label)
          (let ((spec (enotion/wf--local-get1 root group label)))
            (enotion/wf--local-del root group label)
            (enotion/wf--local-add root group new spec)
            (enotion/wf--treemacs-refresh)
            (message "Renamed (local): %s / %s -> %s" group label new))
        (let ((spec (enotion/wf--global-get group label)))
          (unless spec (user-error "Not found in global"))
          (enotion/wf--global-del group label)
          (enotion/wf--global-add group new spec)
          (enotion/wf--save-global)
          (enotion/wf--treemacs-refresh)
          (message "Renamed (global): %s / %s -> %s" group label new))))))

(defun enotion/wf-treemacs-edit-local-file ()
  "Treemacs: e (open/edit local .workflows.el for current treemacs project)."
  (interactive)
  (let* ((ctx (enotion/wf--ctx))
         (root (or (plist-get ctx :root) (enotion/wf--project-root)))
         (file (enotion/wf--local-file root)))
    (unless (file-exists-p file)
      (enotion/wf--local-set root (or (enotion/wf--local-get root) '())))
    (find-file file)
    (message "Edit local workflows: %s" file)))

(defun enotion/wf-treemacs-edit-global ()
  "Treemacs: E (edit global `enotion/workflows`)."
  (interactive)
  (customize-variable 'enotion/workflows))

(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map (kbd "RET") #'treemacs-RET-action)
  (define-key treemacs-mode-map (kbd "a") #'enotion/wf-treemacs-add-local)
  (define-key treemacs-mode-map (kbd "A") #'enotion/wf-treemacs-add-global)
  (define-key treemacs-mode-map (kbd "d") #'enotion/wf-treemacs-delete)
  (define-key treemacs-mode-map (kbd "r") #'enotion/wf-treemacs-rename)
  (define-key treemacs-mode-map (kbd "e") #'enotion/wf-treemacs-edit-local-file)
  (define-key treemacs-mode-map (kbd "E") #'enotion/wf-treemacs-edit-global))

(provide 'treemacs-workflows)

;;; treemacs-workflows.el ends here
