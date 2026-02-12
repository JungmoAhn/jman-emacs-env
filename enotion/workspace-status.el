;;; workspace-status.el --- Magit-style workspace status for Org -*- lexical-binding: t; -*-

;; Author: Jungmo Ahn
;; Version: 0.3
;; Package-Requires: ((emacs "27.1") (magit-section "3.0.0"))
;; Keywords: outlines, org, convenience

;;; Commentary:
;;
;; A Magit-section powered "status" buffer for an Org workflow.
;; Sections:
;;   Inbox (#N)
;;   Tasks (#N)
;;   Projects Active/Maint/Archived (#N)
;;   Projects Blocked (#N) [Hybrid: Manual+Auto]
;;     - Missing project homes (#N)
;;   Topics (#N)
;;
;; Key highlights:
;;   TAB   Open associated file (file-level)
;;   RET   Visit exact entry (entry-level)
;;   g     Refresh
;;   ?     Dispatch/help (transient if available)
;;   s/S   Stage / Stage all (Inbox/Tasks -> Project)
;;   u/U   Fallback chain (Project task -> Tasks -> Inbox)
;;   b     Toggle BLOCK for tasks
;;   n     Set NEXT
;;   t     Set TODO
;;   k     Archive/Delete subtree
;;   a/m/f Set project status Active/Maint/Finished
;;   B     Toggle manual blocked (proj_blocked) on project
;;   x     Show project blockers (agenda view)
;;   v     Tags view for proj_tag (agenda scope)
;;   C     Toggle auto-blocked scope (project-file <-> agenda-tag)
;;   p     Create project home (for missing-project item)
;;   z/Z   Toggle section / all sections folding
;;   N     New item in Inbox/Tasks/Topics

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'magit-section)

(defgroup workspace-status nil
  "Magit-style status buffer for an Org workspace."
  :group 'org)

(defcustom workspace-status-inbox-file (expand-file-name "~/org/inbox.org")
  "Path to inbox file."
  :type 'file)

(defcustom workspace-status-tasks-file (expand-file-name "~/org/tasks.org")
  "Path to tasks file."
  :type 'file)

(defcustom workspace-status-projects-dir (expand-file-name "~/org/roam/projects")
  "Directory containing project home files."
  :type 'directory)

(defcustom workspace-status-topics-dir (expand-file-name "~/org/roam/topics")
  "Directory containing topic files (always included)."
  :type 'directory)

(defcustom workspace-status-projects-recursive t
  "If non-nil, scan project homes recursively under `workspace-status-projects-dir'."
  :type 'boolean)

(defcustom workspace-status-topic-search-dirs
  (list (expand-file-name "~/org/roam/topics")
        (expand-file-name "~/org/roam"))
  "Directories to scan (shallow) for topic notes.

A file is a topic if #+filetags contains :topic:. Files inside
`workspace-status-topics-dir' are included regardless of filetags."
  :type '(repeat directory))

(defcustom workspace-status-project-destination-heading "Tasks"
  "Heading in project files to stage tasks under."
  :type 'string)

(defcustom workspace-status-tasks-destination-heading "Tasks"
  "Heading in tasks file used as fallback destination for project tasks."
  :type 'string)

(defcustom workspace-status-inbox-destination-heading "Inbox"
  "Heading in inbox file used as fallback destination for tasks."
  :type 'string)

(defcustom workspace-status-open-file-other-window t
  "If non-nil, open files in other window."
  :type 'boolean)

(defcustom workspace-status-kill-action 'delete
  "What `k' does: 'archive or 'delete."
  :type '(choice (const :tag "Archive subtree" archive)
                 (const :tag "Delete subtree" delete)))

(defcustom workspace-status-project-template
  "#+title: %s\n#+filetags: :project:proj_%s:proj_active:\n\n* Overview\n- Goal:\n- Scope:\n\n* Today\n** NEXT\n** BLOCK\n** WAIT\n\n* This Week\n- Milestones:\n\n* CI/HIL (Jenkins/LAVA)\n- Jenkins:\n- LAVA:\n- Evidence:\n\n* Decisions\n\n* Links\n- Repo:\n- Runbook:\n\n* Notes\n"
  "Template for new project home files.
Arguments: title, slug."
  :type 'string)

(defcustom workspace-status-prefer-external-project-template t
  "If non-nil, prefer external project template functions when available."
  :type 'boolean)

(defcustom workspace-status-project-filename-format "%Y%m%d%H%M%S"
  "Time format used for project filenames."
  :type 'string)

(defcustom workspace-status-auto-blocked-scope 'project-file
  "Scope for auto-blocked detection.

- project-file: count BLOCK tasks only inside the project home file.
- agenda-tag: count BLOCK tasks across `org-agenda-files' matching proj_tag."
  :type '(choice (const :tag "Project file only" project-file)
                 (const :tag "Across agenda by tag" agenda-tag)))

(defcustom workspace-status-max-project-subtasks 30
  "Max number of project subtasks to render per project per sub-section."
  :type 'integer)

(defcustom workspace-status-show-project-subsections nil
  "If non-nil, show Next/Blockers subsections under project lines."
  :type 'boolean)

(defface workspace-status-dim-face '((t :inherit shadow))
  "Dim face for metadata.")
(defface workspace-status-todo-face '((t :inherit font-lock-keyword-face))
  "Face for TODO keywords.")
(defface workspace-status-project-face '((t :inherit font-lock-function-name-face))
  "Face for project names.")
(defface workspace-status-warn-face '((t :inherit warning))
  "Face for warnings.")
(defface workspace-status-missing-face '((t :inherit error :foreground "red"))
  "Face for missing project tags.")
(defface workspace-status-block-face '((t :inherit error))
  "Face for block labels.")
(defface workspace-status-label-face '((t :foreground "#6FA8DC"))
  "Face for summary labels.")
(defface workspace-status-count-face '((t :foreground "#93C47D"))
  "Face for summary counts.")

(cl-defstruct workspace-status-item
  type file pos title todo tags proj-tag blocker-count project-status meta)

;;; ---------- Low-level helpers ----------

(defun workspace-status--line-width ()
  "Return a reasonable width for aligning lines."
  (let ((win (get-buffer-window (current-buffer) 0)))
    (or (and (window-live-p win) (window-width win))
        (and (window-live-p (selected-window)) (window-width (selected-window)))
        100)))

(defun workspace-status--truncate-to (s max-len)
  "Return S truncated to MAX-LEN with an ASCII ellipsis if needed."
  (if (and (stringp s) (> (string-width s) max-len) (> max-len 3))
      (concat (truncate-string-to-width s (- max-len 3)) "...")
    s))

(defun workspace-status--project-goal (file)
  "Return a short project goal string from FILE, or nil."
  (with-temp-buffer
    (insert-file-contents file nil 0 8192)
    (goto-char (point-min))
    (cond
     ((re-search-forward "^#\\+goal:[ \t]*\\(.*\\)$" nil t)
      (string-trim (match-string 1)))
     ((re-search-forward "^#\\+subtitle:[ \t]*\\(.*\\)$" nil t)
      (string-trim (match-string 1)))
     ((re-search-forward "^\\*+[ \t]+Goal\\b\\(.*\\)$" nil t)
      (string-trim (match-string 1)))
     (t nil))))

(defun workspace-status--org-roam-project-template ()
  "Return org-roam project template (key \"p\") when available."
  (when (boundp 'org-roam-capture-templates)
    (cl-find-if (lambda (tpl) (and (consp tpl) (equal (car tpl) "p")))
                org-roam-capture-templates)))

(defun workspace-status--slugify (s)
  "Return a filename-safe slug for S."
  (let* ((s (downcase (string-trim s)))
         (s (replace-regexp-in-string "[^a-z0-9]+" "_" s))
         (s (replace-regexp-in-string "^_+\\|_+$" "" s)))
    (if (string-empty-p s) "topic" s)))

(defun workspace-status--external-project-home (title)
  "Create a project home using external template if available.
Return created file path, or nil if not used."
  (when (and workspace-status-prefer-external-project-template
             (or (fboundp 'org-roam-capture-) (fboundp 'jacob/create-project-home)))
    (cond
     ((and (fboundp 'org-roam-capture-)
           (workspace-status--org-roam-project-template))
      (let ((template (workspace-status--org-roam-project-template))
            (node (org-roam-node-create :title title)))
        (org-roam-capture- :node node
                           :templates (list template)
                           :props '(:immediate-finish t))
        (ignore-errors (org-roam-node-file node))))
     ((fboundp 'jacob/create-project-home)
      (let* ((slug (if (fboundp 'jacob/proj--slugify)
                       (jacob/proj--slugify title)
                     (let* ((s (downcase (string-trim title)))
                            (s (replace-regexp-in-string "[^a-z0-9]+" "_" s))
                            (s (replace-regexp-in-string "^_+\\|_+$" "" s)))
                       s)))
             (dir (and (boundp 'jacob/org-roam-projects-dir) jacob/org-roam-projects-dir))
             (file (and dir (expand-file-name (format "%s.org" slug) dir))))
        (funcall 'jacob/create-project-home title)
        file))
     (t nil))))

(defun workspace-status--item-p (v)
  "Return non-nil if V is a workspace-status item."
  (and (recordp v) (eq (type-of v) 'workspace-status-item)))

(defun workspace-status--valid-org-file-p (file)
  "Return non-nil if FILE is a regular, non-hidden .org file."
  (let ((base (file-name-nondirectory file)))
    (and (string-match-p "\\.org\\'" base)
         (file-regular-p file)
         (not (string-prefix-p "." base)))))

(defun workspace-status--list-org-files-recursive (dir)
  "Return list of .org files under DIR, recursively if supported."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (when (file-directory-p dir)
      (if (fboundp 'directory-files-recursively)
          (directory-files-recursively dir "\\.org\\'")
        (directory-files dir t "\\.org\\'")))))

(defun workspace-status--read-filetags (file)
  "Return raw filetags string from FILE's #+filetags line, or empty string."
  (with-temp-buffer
    (insert-file-contents file nil 0 4096)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+filetags:[ \t]*\\(.*\\)$" nil t)
        (string-trim (match-string 1))
      "")))

(defun workspace-status--tags-from-filetags (raw)
  "Parse RAW filetags like :a:b: into a list."
  (let ((s (replace-regexp-in-string "[ \t]+" "" (or raw ""))))
    (when (and (stringp s) (> (length s) 0))
      (cl-remove-if #'string-empty-p
                    (split-string (replace-regexp-in-string "^:+\\|:+$" "" s) ":" t)))))

(defun workspace-status--open-file (file &optional pos)
  "Open FILE, optionally jumping to POS."
  (let ((buf (find-file-noselect file)))
    (if workspace-status-open-file-other-window
        (pop-to-buffer buf)
      (switch-to-buffer buf))
    (when pos
      (goto-char pos)
      (org-reveal)
      (org-show-entry))))

(defun workspace-status--pos-or-error (it)
  "Return item position or raise a user error."
  (let ((pos (workspace-status-item-pos it)))
    (unless (and pos (integer-or-marker-p pos))
      (user-error "Item has no position"))
    pos))

(defun workspace-status--ensure-heading (file heading)
  "Ensure FILE has a top-level HEADING. Return insertion point after subtree."
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
      (goto-char (point-min))
      (if (re-search-forward (format "^\\*+[ \t]+%s[ \t]*$" (regexp-quote heading)) nil t)
          (progn
            (org-back-to-heading t)
            (org-end-of-subtree t t)
            (point))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "* " heading "\n")
        (point)))))

(defun workspace-status--heading-level (file heading)
  "Return outline level for HEADING in FILE, or nil."
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
      (goto-char (point-min))
      (when (re-search-forward (format "^\\*+\\s-+%s\\b" (regexp-quote heading)) nil t)
        (org-back-to-heading t)
        (org-outline-level)))))

(defun workspace-status--adjust-subtree-level (subtree target-level)
  "Adjust SUBTREE so its first heading is TARGET-LEVEL."
  (let ((m (string-match "^\\(\\*+\\)" subtree)))
    (if (not m)
        subtree
      (let* ((cur (length (match-string 1 subtree)))
             (delta (- target-level cur)))
        (if (<= delta 0)
            subtree
          (replace-regexp-in-string
           "^\\(\\*+\\)"
           (lambda (s) (concat (make-string delta ?*) s))
           subtree))))))

(defun workspace-status--cut-subtree-at (file pos)
  "Cut subtree at FILE POS, return subtree string."
  (unless (and pos (integer-or-marker-p pos))
    (user-error "Item has no position"))
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
      (goto-char pos)
      (org-back-to-heading t)
      (let ((beg (point)))
        (org-end-of-subtree t t)
        (let ((sub (buffer-substring-no-properties beg (point))))
          (delete-region beg (point))
          (save-buffer)
          sub)))))

(defun workspace-status--paste-subtree-under (file heading subtree)
  "Paste SUBTREE under HEADING in FILE."
  (let* ((insert-pos (workspace-status--ensure-heading file heading))
         (level (or (workspace-status--heading-level file heading) 1))
         (subtree (workspace-status--adjust-subtree-level subtree (1+ level))))
    (with-current-buffer (find-file-noselect file)
      (goto-char insert-pos)
      (unless (bolp) (insert "\n"))
      (insert (string-trim-right subtree) "\n")
      (save-buffer))))

(defun workspace-status--goto-heading-pos (file heading)
  "Return position of HEADING in FILE, or nil."
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
      (goto-char (point-min))
      (when (re-search-forward (format "^\\*+[ \t]+%s[ \t]*$" (regexp-quote heading)) nil t)
        (org-back-to-heading t)
        (point)))))

;;; ---------- Collectors ----------

(defun workspace-status--org-items-in-file (file &optional match type)
  "Collect TODO entries from FILE.

MATCH is an org-map-entries matcher.
TYPE is one of 'inbox or 'task."
  (let ((file (expand-file-name file)))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
          (goto-char (point-min))
          (let ((items '())
                (m (or match "+TODO<>\"\"")))
            (org-map-entries
             (lambda ()
               (let* ((pos (point))
                      (todo (org-get-todo-state))
                      (title (org-get-heading t t t t))
                      (tags (org-get-tags))
                      (proj (cl-find-if (lambda (t) (string-prefix-p "proj_" t)) tags)))
                 (when (and (not proj) (stringp title))
                   (when (string-match "\\(:proj_[A-Za-z0-9_-]+:?\\)" title)
                     (let* ((raw (match-string 1 title))
                            (tag (replace-regexp-in-string "^:+\\|:+$" "" raw)))
                       (setq title (string-trim (replace-regexp-in-string (regexp-quote raw) "" title)))
                       (setq proj tag)
                       (setq tags (cons tag tags)))))
                 (push (make-workspace-status-item
                        :type (or type 'task)
                        :file file :pos pos :title title
                        :todo todo :tags tags :proj-tag proj)
                       items)))
             m 'file)
            (nreverse items)))))))

(defun workspace-status--project-info (file)
  "Return plist for project home FILE, or nil."
  (let* ((raw (workspace-status--read-filetags file))
         (tags (workspace-status--tags-from-filetags raw)))
    (when (member "project" tags)
      (let* ((proj-tag (cl-find-if (lambda (t) (string-prefix-p "proj_" t)) tags))
             (name (file-name-base file))
             (status (cond ((member "proj_archived" tags) 'archived)
                           ((member "proj_maint" tags) 'maint)
                           ((member "proj_active" tags) 'active)
                           (t 'active)))
             (manual-blocked (member "proj_blocked" tags)))
        (list :file file :name name :tags tags :proj-tag proj-tag
              :status status :manual-blocked manual-blocked)))))

(defun workspace-status--collect-projects ()
  "Return list of project plist."
  (let* ((dir (file-name-as-directory (expand-file-name workspace-status-projects-dir)))
         (files (and (file-directory-p dir)
                     (if workspace-status-projects-recursive
                         (workspace-status--list-org-files-recursive dir)
                       (directory-files dir t "\\.org\\'"))))
         (files (cl-remove-if-not #'workspace-status--valid-org-file-p files)))
    (cl-remove-if-not #'identity (mapcar #'workspace-status--project-info files))))

(defun workspace-status--known-proj-tags (projects)
  "Return hash table of known proj tags from PROJECTS."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (p projects)
      (let ((tag (plist-get p :proj-tag)))
        (when (and tag (not (string-empty-p tag)))
          (puthash tag p h))))
    h))

(defun workspace-status--missing-project-tags (items known-tags)
  "Return list of missing project tags from ITEMS not in KNOWN-TAGS."
  (let ((seen (make-hash-table :test 'equal))
        (out '()))
    (dolist (it items)
      (let ((tag (workspace-status-item-proj-tag it)))
        (when (and tag (not (gethash tag known-tags)) (not (gethash tag seen)))
          (puthash tag t seen)
          (push tag out))))
    (nreverse out)))

(defun workspace-status--project-task-stats (proj)
  "Return plist (:next N :todo T :block B) for project PROJ plist."
  (let* ((project-file (plist-get proj :file))
         (proj-tag (plist-get proj :proj-tag))
         (n-next 0) (n-todo 0) (n-block 0))
    (with-current-buffer (find-file-noselect project-file)
      (org-with-wide-buffer
        (let ((org-element-use-cache nil)
              (scoped nil))
          ;; Prefer counting only under "* Tasks" if present.
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward "^\\*+\\s-+Tasks\\b" nil t)
              (org-narrow-to-subtree)
              (setq scoped t)))
          (org-map-entries
           (lambda ()
             (pcase (org-get-todo-state)
               ("NEXT" (setq n-next (1+ n-next)))
               ("BLOCK" (setq n-block (1+ n-block)))
               ((or "TODO" "WAIT") (setq n-todo (1+ n-todo)))))
           "+TODO<>\"\"" 'file)
          (when scoped
            (widen)))))
    (when (and (eq workspace-status-auto-blocked-scope 'agenda-tag) proj-tag)
      (dolist (af (org-agenda-files))
        (with-current-buffer (find-file-noselect af)
          (org-with-wide-buffer
            (let ((org-element-use-cache nil))
              (org-map-entries
               (lambda ()
                 (when (workspace-status--heading-has-proj-tag-p proj-tag)
                   (pcase (org-get-todo-state)
                     ("NEXT" (setq n-next (1+ n-next)))
                     ("BLOCK" (setq n-block (1+ n-block)))
                     ((or "TODO" "WAIT") (setq n-todo (1+ n-todo))))))
               "+TODO<>\"\"" 'file))))))
    (list :next n-next :todo n-todo :block n-block)))

(defun workspace-status--collect-project-tasks (project-file todo-state &optional limit proj-tag)
  "Collect up to LIMIT tasks with TODO-STATE for project."
  (let ((limit (or limit workspace-status-max-project-subtasks))
        (out '()))
    (with-current-buffer (find-file-noselect project-file)
      (org-with-wide-buffer
        (let ((org-element-use-cache nil))
          (org-map-entries
           (lambda ()
             (when (< (length out) limit)
               (let* ((pos (point))
                      (todo (org-get-todo-state))
                      (title (org-get-heading t t t t))
                      (tags (org-get-tags))
                      (proj (or (cl-find-if (lambda (t) (string-prefix-p "proj_" t)) tags)
                                proj-tag)))
                 (push (make-workspace-status-item
                        :type 'project-task
                        :file project-file :pos pos
                        :title title :todo todo
                        :tags tags :proj-tag proj)
                       out))))
           (format "TODO=\"%s\"" todo-state) 'file))))
    (when (and (eq workspace-status-auto-blocked-scope 'agenda-tag) proj-tag)
      (dolist (af (org-agenda-files))
        (with-current-buffer (find-file-noselect af)
          (org-with-wide-buffer
            (let ((org-element-use-cache nil))
              (org-map-entries
               (lambda ()
                 (when (and (< (length out) limit)
                            (string= (org-get-todo-state) todo-state)
                            (workspace-status--heading-has-proj-tag-p proj-tag))
                   (let* ((pos (point))
                          (todo (org-get-todo-state))
                          (title (org-get-heading t t t t))
                          (tags (org-get-tags))
                          (proj (or (cl-find-if (lambda (t) (string-prefix-p "proj_" t)) tags)
                                    proj-tag)))
                     (push (make-workspace-status-item
                            :type 'project-task
                            :file af :pos pos
                            :title title :todo todo
                            :tags tags :proj-tag proj)
                           out))))
               "+TODO<>\"\"" 'file))))))
    (nreverse out)))

(defun workspace-status--project-blocker-count (proj)
  "Return count of BLOCK TODOs for project PROJ plist."
  (plist-get (workspace-status--project-task-stats proj) :block))

(defun workspace-status--topics ()
  "Return list of topic items."
  (let ((seen (make-hash-table :test 'equal))
        (out '()))
    (dolist (d workspace-status-topic-search-dirs)
      (let* ((dir (file-name-as-directory (expand-file-name d)))
             (files (cl-remove-if-not #'workspace-status--valid-org-file-p
                                      (or (workspace-status--list-org-files-recursive dir) '()))))
        (dolist (f files)
          (unless (gethash f seen)
            (puthash f t seen)
            (let* ((raw (workspace-status--read-filetags f))
                   (tags (workspace-status--tags-from-filetags raw))
                   (is-topic (or (file-in-directory-p f (expand-file-name workspace-status-topics-dir))
                                 (member "topic" tags))))
              (when is-topic
                (push (make-workspace-status-item :type 'topic :file f :pos 1 :title (file-name-base f)) out)))))))
    (nreverse out)))

;;; ---------- Project status mutations ----------

(defun workspace-status--set-project-status (file status)
  "Set project FILE status tag to STATUS."
  (let* ((raw (workspace-status--read-filetags file))
         (tags (workspace-status--tags-from-filetags raw))
         (tags (cl-remove-if (lambda (t) (member t '("proj_active" "proj_maint" "proj_archived"))) tags))
         (tags (append tags (list (pcase status
                                   ('active "proj_active")
                                   ('maint "proj_maint")
                                   ('archived "proj_archived")
                                   (_ "proj_active"))))))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^#\\+filetags:.*$" nil t)
            (replace-match (concat "#+filetags: :" (mapconcat #'identity tags ":") ":") t t)
          (insert (concat "#+filetags: :" (mapconcat #'identity tags ":") ":\n")))
        (save-buffer)))))

(defun workspace-status--toggle-project-blocked (file)
  "Toggle manual proj_blocked tag in project FILE."
  (let* ((raw (workspace-status--read-filetags file))
         (tags (workspace-status--tags-from-filetags raw))
         (has (member "proj_blocked" tags))
         (tags (if has (delete "proj_blocked" tags) (append tags '("proj_blocked")))))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "^#\\+filetags:.*$" nil t)
            (replace-match (concat "#+filetags: :" (mapconcat #'identity tags ":") ":") t t)
          (insert (concat "#+filetags: :" (mapconcat #'identity tags ":") ":\n")))
        (save-buffer))
      (message "Manual blocked: %s" (if has "OFF" "ON")))))

;;; ---------- Actions (interactive) ----------

(defun workspace-status--item-at-point ()
  (let ((sec (magit-current-section)))
    (while (and sec
                (or (not (slot-boundp sec 'value))
                    (not (workspace-status--item-p (oref sec value)))))
      (setq sec (oref sec parent)))
    (when (and sec (slot-boundp sec 'value))
      (oref sec value))))

(defun workspace-status--items-section-title ()
  "Return the current ws-items section title, or nil."
  (let ((sec (magit-current-section)))
    (while (and sec (not (eq (oref sec type) 'ws-items)))
      (setq sec (oref sec parent)))
    (when sec
      (oref sec value))))

(defun workspace-status--heading-has-proj-tag-p (proj-tag)
  "Return non-nil if current heading has PROJ-TAG in tags or title."
  (when (and proj-tag (stringp proj-tag))
    (let ((tags (org-get-tags))
          (title (org-get-heading t t t t)))
      (or (member proj-tag tags)
          (and (stringp title)
               (string-match (format "\\(:%s:?\\)" (regexp-quote proj-tag)) title))))))

(defun workspace-status-open-file ()
  "Open associated file for the item at point."
  (interactive)
  (let ((it (workspace-status--item-at-point)))
    (unless it (user-error "No item here"))
    (workspace-status--open-file (workspace-status-item-file it) nil)))

(defun workspace-status-tab ()
  "TAB: toggle section if on a heading, else open item file."
  (interactive)
  (let* ((sec (magit-current-section))
         (val (and sec (slot-boundp sec 'value) (oref sec value))))
    (if (workspace-status--item-p val)
        (workspace-status-open-file)
      (workspace-status-section-toggle))))

(defun workspace-status-visit-entry ()
  "RET: visit exact entry.

For project items, jumps to the configured destination heading (Tasks) if present."
  (interactive)
  (let ((it (workspace-status--item-at-point)))
    (unless it (user-error "No item here"))
    (pcase (workspace-status-item-type it)
      ('project
       (let ((pos (or (workspace-status--goto-heading-pos (workspace-status-item-file it)
                                                         workspace-status-project-destination-heading)
                      1)))
         (workspace-status--open-file (workspace-status-item-file it) pos)))
      (_
       (workspace-status--open-file (workspace-status-item-file it)
                                    (workspace-status--pos-or-error it))))))

(defun workspace-status-add-item ()
  "Add a new item in the current Inbox/Tasks/Topics section."
  (interactive)
  (let* ((section (workspace-status--items-section-title))
         (title (read-string "New item: ")))
    (unless (and section (not (string-empty-p title)))
      (user-error "Place point on Inbox/Tasks/Topics section and enter a title"))
    (pcase section
      ("Inbox"
       (workspace-status--paste-subtree-under
        workspace-status-inbox-file
        workspace-status-inbox-destination-heading
        (format "* TODO %s\n" title))
       (message "Added to Inbox: %s" title))
      ("Tasks"
       (workspace-status--paste-subtree-under
        workspace-status-tasks-file
        workspace-status-tasks-destination-heading
        (format "* TODO %s\n" title))
       (message "Added to Tasks: %s" title))
      ("Topics"
       (let* ((dir (expand-file-name workspace-status-topics-dir))
              (slug (workspace-status--slugify title))
              (file (expand-file-name (format "%s.org" slug) dir)))
         (make-directory dir t)
         (with-current-buffer (find-file-noselect file)
           (unless (file-exists-p file)
             (erase-buffer)
             (insert "#+title: " title "\n")
             (insert "#+filetags: :topic:\n\n")
             (insert "* " title "\n")
             (save-buffer)))
         (message "Created topic: %s" file)))
      (_ (user-error "N works only in Inbox/Tasks/Topics sections")))
    (workspace-status-refresh)))

(defun workspace-status--project-choices (projects)
  "Return alist (display . project-plist)."
  (mapcar
   (lambda (p)
     (let* ((name (plist-get p :name))
            (tag (or (plist-get p :proj-tag) ""))
            (disp (if (string-empty-p tag) name (format "%s  (%s)" name tag))))
       (cons disp p)))
   projects))

(defun workspace-status--select-project (projects &optional prefer-tag)
  "Select a project plist from PROJECTS.
If PREFER-TAG matches an existing project, return it."
  (or (and prefer-tag
           (cl-find-if (lambda (p) (string= prefer-tag (plist-get p :proj-tag))) projects))
      (let* ((choices (workspace-status--project-choices projects)))
        (cdr (assoc (completing-read "Project: " choices nil t) choices)))))

(defun workspace-status-create-project-home (&optional proj-tag)
  "Create a project home file.
If PROJ-TAG provided (e.g. proj_rtos), uses it."
  (interactive)
  (let* ((tag (or proj-tag (read-string "proj tag (e.g. proj_rtos): ")))
         (tag (if (string-prefix-p "proj_" tag) tag (concat "proj_" tag)))
         (tag-slug (replace-regexp-in-string "^proj_" "" tag))
         (title (read-string "Project title: " tag-slug))
         (external (workspace-status--external-project-home title)))
    (if external
        (progn
          (message "Created project: %s" external)
          external)
      (let* ((slug (replace-regexp-in-string "[^[:alnum:][:nonascii:]_.-]+" "_" (downcase title)))
             (slug (replace-regexp-in-string "_+" "_" slug))
             (slug (replace-regexp-in-string "^_+\\|_+$" "" slug))
             (slug (if (string-empty-p slug) tag-slug slug))
             (ts (format-time-string workspace-status-project-filename-format))
             (fname (format "%s-%s.org" ts slug))
             (dir (file-name-as-directory (expand-file-name workspace-status-projects-dir)))
             (file (expand-file-name fname dir)))
        (unless (file-directory-p dir) (make-directory dir t))
        (when (and (file-exists-p file)
                   (not (yes-or-no-p (format "File exists: %s. Overwrite? " file))))
          (user-error "Cancelled"))
        (with-temp-file file
          (insert (format workspace-status-project-template title tag-slug)))
        (message "Created project: %s" file)
        file))))

(defun workspace-status-create-project-from-missing ()
  "Create project home for missing project tag at point."
  (interactive)
  (let ((it (workspace-status--item-at-point)))
    (unless (and it (memq (workspace-status-item-type it) '(task inbox)))
      (user-error "Not a task item"))
    (let ((tag (workspace-status-item-proj-tag it)))
      (unless (and tag (not (string-empty-p tag)))
        (user-error "No proj tag on this item"))
      (workspace-status-create-project-home tag)
      (workspace-status-refresh))))

(defun workspace-status-stage ()
  "Stage current Inbox/Tasks item into a project."
  (interactive)
  (let ((it (workspace-status--item-at-point)))
    (unless it (user-error "No item here"))
    (unless (memq (workspace-status-item-type it) '(inbox task))
      (user-error "Stage is only for Inbox/Tasks items"))
    (let* ((projects (workspace-status--collect-projects))
           (known (workspace-status--known-proj-tags projects))
           (prefer (workspace-status-item-proj-tag it))
           (proj (cond
                  ((and prefer (gethash prefer known)) (gethash prefer known))
                  (prefer
                   (when (yes-or-no-p (format "No project home for %s. Create? " prefer))
                     (push (workspace-status--project-info (workspace-status-create-project-home prefer)) projects))
                   (workspace-status--select-project projects nil))
                  (t (workspace-status--select-project projects nil))))
           (proj-file (plist-get proj :file))
           (sub (workspace-status--cut-subtree-at (workspace-status-item-file it) (workspace-status-item-pos it))))
      (workspace-status--paste-subtree-under proj-file workspace-status-project-destination-heading sub)
      (message "Staged to %s" (file-name-nondirectory proj-file))
      (workspace-status-refresh))))

(defun workspace-status-stage-all ()
  "Stage all Inbox/Tasks items in current section."
  (interactive)
  (let* ((sec (magit-current-section))
         (items '())
         (ok 0) (fail 0)
         (projects (workspace-status--collect-projects))
         (known (workspace-status--known-proj-tags projects)))
    (unless sec (user-error "No section"))
    (magit-section-map-sections
     (lambda (s)
       (let ((v (and s (slot-boundp s 'value) (oref s value))))
         (when (and (workspace-status--item-p v)
                    (memq (workspace-status-item-type v) '(inbox task)))
           (push v items))))
     sec)
    (dolist (it (nreverse items))
      (condition-case err
          (progn
            (let* ((prefer (workspace-status-item-proj-tag it))
                   (proj (if (and prefer (gethash prefer known))
                             (gethash prefer known)
                           (workspace-status--select-project projects prefer)))
                   (proj-file (plist-get proj :file))
                   (sub (workspace-status--cut-subtree-at (workspace-status-item-file it) (workspace-status-item-pos it))))
              (workspace-status--paste-subtree-under proj-file workspace-status-project-destination-heading sub)
              (setq ok (1+ ok))))
        (error
         (setq fail (1+ fail))
         (message "Stage failed: %s" (error-message-string err)))))
    (workspace-status-refresh)
    (message "Stage all: %d ok, %d failed" ok fail)))

(defun workspace-status-fallback ()
  "Fallback/unstage current item.

- project-task -> tasks.org
- task -> inbox.org
- inbox -> no-op"
  (interactive)
  (let ((it (workspace-status--item-at-point)))
    (unless it (user-error "No item here"))
    (pcase (workspace-status-item-type it)
      ('project-task
       (let ((sub (workspace-status--cut-subtree-at (workspace-status-item-file it) (workspace-status-item-pos it))))
         (workspace-status--paste-subtree-under workspace-status-tasks-file workspace-status-tasks-destination-heading sub)
         (message "Moved to Tasks")
         (workspace-status-refresh)))
      ('task
       (let ((sub (workspace-status--cut-subtree-at (workspace-status-item-file it) (workspace-status-item-pos it))))
         (workspace-status--paste-subtree-under workspace-status-inbox-file workspace-status-inbox-destination-heading sub)
         (message "Moved to Inbox")
         (workspace-status-refresh)))
      ('inbox (message "Inbox item: no further fallback"))
      (_ (user-error "Fallback applies to project/task/inbox items")))))

(defun workspace-status-force-to-inbox ()
  "Force move current project-task/task/inbox item to Inbox."
  (interactive)
  (let ((it (workspace-status--item-at-point)))
    (unless it (user-error "No item here"))
    (unless (memq (workspace-status-item-type it) '(project-task task inbox))
      (user-error "Not a movable item"))
    (let ((sub (workspace-status--cut-subtree-at (workspace-status-item-file it) (workspace-status-item-pos it))))
      (workspace-status--paste-subtree-under workspace-status-inbox-file workspace-status-inbox-destination-heading sub)
      (message "Moved to Inbox")
      (workspace-status-refresh))))

(defun workspace-status-toggle-block ()
  "Toggle BLOCK state for current task-like item."
  (interactive)
  (let ((it (workspace-status--item-at-point)))
    (unless it (user-error "No item here"))
    (unless (memq (workspace-status-item-type it) '(inbox task project-task))
      (user-error "BLOCK toggle applies to task items"))
    (with-current-buffer (find-file-noselect (workspace-status-item-file it))
      (org-with-wide-buffer
        (goto-char (workspace-status--pos-or-error it))
        (org-back-to-heading t)
        (let ((cur (org-get-todo-state)))
          (if (string= cur "BLOCK")
              (org-todo "TODO")
            (org-todo "BLOCK"))
          (save-buffer)
          (message "TODO: %s" (org-get-todo-state)))))
    (workspace-status-refresh)))

(defun workspace-status-set-next ()
  "Set TODO state to NEXT for task-like items."
  (interactive)
  (let ((it (workspace-status--item-at-point)))
    (unless (and it (memq (workspace-status-item-type it) '(inbox task project-task)))
      (user-error "Not a task item"))
    (with-current-buffer (find-file-noselect (workspace-status-item-file it))
      (org-with-wide-buffer
        (goto-char (workspace-status--pos-or-error it))
        (org-back-to-heading t)
        (org-todo "NEXT")
        (save-buffer)
        (message "TODO: %s" (org-get-todo-state))))
    (workspace-status-refresh)))

(defun workspace-status-set-todo ()
  "Set TODO state to TODO for task-like items."
  (interactive)
  (let ((it (workspace-status--item-at-point)))
    (unless (and it (memq (workspace-status-item-type it) '(inbox task project-task)))
      (user-error "Not a task item"))
    (with-current-buffer (find-file-noselect (workspace-status-item-file it))
      (org-with-wide-buffer
        (goto-char (workspace-status--pos-or-error it))
        (org-back-to-heading t)
        (org-todo "TODO")
        (save-buffer)
        (message "TODO: %s" (org-get-todo-state))))
    (workspace-status-refresh)))

(defun workspace-status-kill ()
  "Archive/delete current item subtree."
  (interactive)
  (let ((it (workspace-status--item-at-point)))
    (unless it (user-error "No item here"))
    (pcase (workspace-status-item-type it)
      ((or 'inbox 'task 'project-task)
       (with-current-buffer (find-file-noselect (workspace-status-item-file it))
         (org-with-wide-buffer
           (goto-char (workspace-status--pos-or-error it))
           (org-back-to-heading t)
           (pcase workspace-status-kill-action
             ('archive
              (when (y-or-n-p "Archive this subtree? ")
                (org-archive-subtree)
                (save-buffer)
                (message "Archived")))
             ('delete
              (when (y-or-n-p "Delete this subtree? ")
                (let ((beg (point)))
                  (org-end-of-subtree t t)
                  (delete-region beg (point))
                  (save-buffer)
                  (message "Deleted"))))))))
      ('project
       (pcase workspace-status-kill-action
         ('archive
          (when (y-or-n-p "Finish this project (set proj_archived)? ")
            (workspace-status--set-project-status (workspace-status-item-file it) 'archived)
            (message "Project finished")))
         ('delete
          (when (y-or-n-p "Delete this project file? ")
            (delete-file (workspace-status-item-file it))
            (message "Project file deleted")))))
      (_ (user-error "Kill applies to task or project items")))
    (workspace-status-refresh)))

(defun workspace-status-project-set-active ()
  (interactive)
  (let ((it (workspace-status--item-at-point)))
    (unless (and it (eq (workspace-status-item-type it) 'project)) (user-error "Not a project"))
    (workspace-status--set-project-status (workspace-status-item-file it) 'active)
    (workspace-status-refresh)))

(defun workspace-status-project-set-maint ()
  (interactive)
  (let ((it (workspace-status--item-at-point)))
    (unless (and it (eq (workspace-status-item-type it) 'project)) (user-error "Not a project"))
    (workspace-status--set-project-status (workspace-status-item-file it) 'maint)
    (workspace-status-refresh)))

(defun workspace-status-project-set-finished ()
  (interactive)
  (let ((it (workspace-status--item-at-point)))
    (unless (and it (eq (workspace-status-item-type it) 'project)) (user-error "Not a project"))
    (workspace-status--set-project-status (workspace-status-item-file it) 'archived)
    (workspace-status-refresh)))

(defun workspace-status-project-toggle-blocked ()
  (interactive)
  (let ((it (workspace-status--item-at-point)))
    (unless (and it (eq (workspace-status-item-type it) 'project)) (user-error "Not a project"))
    (workspace-status--toggle-project-blocked (workspace-status-item-file it))
    (workspace-status-refresh)))

(defun workspace-status-project-show-blockers ()
  (interactive)
  (let ((it (workspace-status--item-at-point)))
    (unless (and it (eq (workspace-status-item-type it) 'project)) (user-error "Not a project"))
    (let ((org-agenda-files (list (workspace-status-item-file it))))
      (org-tags-view nil "TODO=\"BLOCK\""))))

(defun workspace-status-view-proj-tag ()
  "Open an agenda tags view for this item's proj_tag."
  (interactive)
  (let ((it (workspace-status--item-at-point)))
    (unless it (user-error "No item here"))
    (let ((tag (workspace-status-item-proj-tag it)))
      (unless (and tag (not (string-empty-p tag)))
        (user-error "No proj_tag on this item"))
      (org-tags-view nil (format "+%s" tag)))))

(defun workspace-status-toggle-auto-blocked-scope ()
  "Toggle auto-blocked scope and refresh."
  (interactive)
  (setq workspace-status-auto-blocked-scope
        (if (eq workspace-status-auto-blocked-scope 'project-file) 'agenda-tag 'project-file))
  (message "Auto-blocked scope: %s" workspace-status-auto-blocked-scope)
  (workspace-status-refresh))

(defun workspace-status-section-toggle ()
  "Toggle current section folding (TAB is open-file)."
  (interactive)
  (magit-section-toggle (magit-current-section)))

(defun workspace-status-sections-toggle-all ()
  "Toggle all sections folding."
  (interactive)
  (magit-section-cycle-diffs))

;;; ---------- Rendering ----------

(defun workspace-status--format-item-line (it &optional extra face)
  "Return a propertized single-line string for item IT."
  (let ((todo (workspace-status-item-todo it))
        (title (workspace-status-item-title it))
        (proj (workspace-status-item-proj-tag it)))
    (let* ((type (workspace-status-item-type it))
           (goal (and (eq type 'project) (workspace-status-item-meta it)))
           (todo-prefix (if (memq type '(project missing-project))
                            ""
                          (propertize (format "%-7s " (or todo "")) 'face 'workspace-status-todo-face)))
           (name (propertize title 'face (pcase type
                                           ('project 'workspace-status-project-face)
                                           ('missing-project 'workspace-status-warn-face)
                                           (_ 'default))))
           (goal-str (and goal (not (string-empty-p goal))
                          (propertize (format " - %s" goal) 'face 'workspace-status-dim-face)))
           (left (concat todo-prefix name (or goal-str "")))
           (missing (and (memq type '(task inbox)) proj
                         (or (workspace-status-item-meta it)
                             (member proj workspace-status--missing-tags))))
           (proj-face (if missing 'workspace-status-missing-face 'workspace-status-dim-face))
           (right (when (or proj extra)
                    (string-trim
                     (concat (when extra (propertize extra 'face (or face 'workspace-status-dim-face)))
                             (when (and extra proj) "  ")
                             (when proj (propertize (format ":: %s" proj) 'face proj-face)))))))
      (if right
          (let* ((width (max 4 (1- (workspace-status--line-width))))
                 (left-max (max 10 (- width (string-width right) 2)))
                 (left (workspace-status--truncate-to left left-max))
                 (pad (- width (string-width left) (string-width right)))
                 (pad (max 2 pad)))
            (concat left (make-string pad ? ) right))
        left))))

(defun workspace-status--insert-items-section (title items)
  (magit-insert-section (ws-items title)
    (magit-insert-heading (format "%s (%d)" title (length items)))
    (dolist (it items)
      (magit-insert-section (item it t)
        (magit-insert-heading (workspace-status--format-item-line it))))))

(defun workspace-status--project-item (p &optional stats)
  (let* ((file (plist-get p :file))
         (name (plist-get p :name))
         (proj-tag (plist-get p :proj-tag))
         (status (plist-get p :status))
         (it (make-workspace-status-item :type 'project :file file :pos 1 :title name :proj-tag proj-tag
                                         :project-status status
                                         :blocker-count (plist-get stats :block)
                                         :meta (workspace-status--project-goal file))))
    it))

(defun workspace-status--insert-project-with-subtasks (p &optional is-blocked show-status)
  "Insert a project line, plus Next/Blockers sub-sections."
  (let* ((file (plist-get p :file))
         (proj-tag (plist-get p :proj-tag))
         (status (plist-get p :status))
         (manual (plist-get p :manual-blocked))
         (bc (plist-get p :blocker-count))
         (auto (plist-get p :auto-blocked))
         (stats (workspace-status--project-task-stats p))
         (it (workspace-status--project-item p stats))
         (next-items (if workspace-status-show-project-subsections
                         (workspace-status--collect-project-tasks file "NEXT" workspace-status-max-project-subtasks proj-tag)
                       nil))
         (block-items (if workspace-status-show-project-subsections
                          (workspace-status--collect-project-tasks file "BLOCK" workspace-status-max-project-subtasks proj-tag)
                        nil))
         (next (plist-get stats :next))
         (todo (plist-get stats :todo))
         (block (plist-get stats :block))
         (warn (and (= next 0) (> todo 0)))
         (label (cond ((and manual auto) "[M+A]")
                      (manual "[M]")
                      (auto "[A]")
                      (is-blocked "[?]")
                      (t "")))
         (status-text (and show-status (format "{%s} " (symbol-name status))))
         (extra (concat
                 (when (and is-blocked (not (string-empty-p label))) (concat label " "))
                 status-text
                 (format "NEXT:%d TODO:%d BLOCK:%d" next todo block))))
    (magit-insert-section (item it t)
      (magit-insert-heading
       (workspace-status--format-item-line
        it extra (if warn 'workspace-status-warn-face (if is-blocked 'workspace-status-block-face 'workspace-status-dim-face))))
      ;; Sub-sections: NEXT and BLOCK (only if present)
      (when next-items
        (magit-insert-section (ws-project-next proj-tag)
          (magit-insert-heading (format "  Next (%d)" (length next-items)))
          (dolist (ti next-items)
            (magit-insert-section (item ti t)
              (magit-insert-heading
               (workspace-status--format-item-line ti (format "(%s)" (file-name-nondirectory file)) 'workspace-status-dim-face))))))
      (when block-items
        (magit-insert-section (ws-project-block proj-tag)
          (magit-insert-heading (format "  Blockers (%d)" (length block-items)))
          (dolist (ti block-items)
            (magit-insert-section (item ti t)
              (magit-insert-heading
               (workspace-status--format-item-line ti (format "(%s)" (file-name-nondirectory file)) 'workspace-status-block-face)))))))))

(defun workspace-status--insert-projects-section (title projects &optional show-status)
  (magit-insert-section (ws-projects title)
    (magit-insert-heading (format "%s (%d)" title (length projects)))
    (dolist (p projects)
      (workspace-status--insert-project-with-subtasks p nil show-status))))

(defun workspace-status--insert-projects-blocked-section (title blocked)
  (magit-insert-section (ws-projects title)
    (magit-insert-heading (format "%s (%d)" title (length blocked)))
    (dolist (p blocked)
      (workspace-status--insert-project-with-subtasks p t t))))

(defun workspace-status--mark-missing-tags (items missing-tags)
  "Mark ITEMS with missing-tags via :meta flag."
  (dolist (it items)
    (when (and (memq (workspace-status-item-type it) '(task inbox))
               (workspace-status-item-proj-tag it)
               (member (workspace-status-item-proj-tag it) missing-tags))
      (setf (workspace-status-item-meta it) t)))
  items)

(defun workspace-status--partition-projects (projects)
  "Return plist of partitioned projects with hybrid blocked."
  (let ((blocked '()) (active '()) (maint '()) (archived '()))
    (dolist (p projects)
      (let* ((manual (plist-get p :manual-blocked))
             (bc (workspace-status--project-blocker-count p))
             (auto (> (or bc 0) 0))
             (status (plist-get p :status))
             (pp (plist-put (copy-sequence p) :blocker-count bc))
             (pp (plist-put pp :auto-blocked auto)))
        (if (or manual auto)
            (push pp blocked)
          (pcase status
            ('archived (push pp archived))
            ('maint (push pp maint))
            (_ (push pp active))))))
    (list :active (nreverse active)
          :maint (nreverse maint)
          :archived (nreverse archived)
          :blocked (nreverse blocked))))

(defun workspace-status--insert-summary (inbox tasks parts topics missing)
  (let* ((a (length (plist-get parts :active)))
         (b (length (plist-get parts :blocked))))
    (let* ((labels '("Inbox" "Tasks" "Active" "Blocked" "Topics" "Missing"))
           (counts (list (length inbox) (length tasks) a b (length topics) (length missing)))
           (pairs (cl-mapcar (lambda (lbl cnt)
                               (format "%s %s"
                                       (propertize lbl 'face 'workspace-status-label-face)
                                       (propertize (number-to-string cnt) 'face 'workspace-status-count-face)))
                             labels counts)))
      (insert (mapconcat #'identity pairs " | "))
      (insert "\n\n"))))

;;; ---------- Refresh ----------

(defun workspace-status-refresh ()
  "Refresh the status buffer."
  (interactive)
  (let* ((line (line-number-at-pos))
         (col (current-column))
         (inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (workspace-status-root)
      (insert (propertize "Workspace Status    [g] refresh   [?] help\n" 'face 'bold))
      (insert "\n")

      (let* ((inbox (workspace-status--org-items-in-file workspace-status-inbox-file nil 'inbox))
             (tasks (workspace-status--org-items-in-file workspace-status-tasks-file nil 'task))
             (projects (workspace-status--collect-projects))
             (parts (workspace-status--partition-projects projects))
             (known (workspace-status--known-proj-tags projects))
             (missing-tags (workspace-status--missing-project-tags (append inbox tasks) known))
             (topics (workspace-status--topics)))

        (setq workspace-status--missing-tags missing-tags)
        (workspace-status--mark-missing-tags tasks missing-tags)
        (workspace-status--mark-missing-tags inbox missing-tags)

        (workspace-status--insert-summary inbox tasks parts topics missing-tags)

        (workspace-status--insert-items-section "Inbox" inbox)
        (workspace-status--insert-items-section "Tasks" tasks)
        (workspace-status--insert-projects-section "Active Projects" (plist-get parts :active) nil)
        (workspace-status--insert-projects-section "Maint Projects" (plist-get parts :maint) t)
        (workspace-status--insert-projects-section "Finished Projects" (plist-get parts :archived) t)
        (workspace-status--insert-projects-blocked-section "Blocked Projects" (plist-get parts :blocked))
        (workspace-status--insert-items-section "Topics" topics)))

    (goto-char (point-min))
    (forward-line (1- (max 1 line)))
    (move-to-column col)))

;;; ---------- Dispatch/help ----------

(defvar workspace-status--has-transient (require 'transient nil t))

(defvar-local workspace-status--missing-tags nil
  "Buffer-local list of missing project tags for rendering.")

(when workspace-status--has-transient
  (transient-define-prefix workspace-status-dispatch ()
    "Workspace Status commands."
    ["Navigation"
     ("g" "Refresh" workspace-status-refresh)
     ("i" "Open inbox" (lambda () (interactive) (workspace-status--open-file workspace-status-inbox-file)))
     ("T" "Open tasks" (lambda () (interactive) (workspace-status--open-file workspace-status-tasks-file)))]
    ["Item"
     ("TAB" "Open file" workspace-status-open-file)
     ("RET" "Visit entry" workspace-status-visit-entry)
     ("s" "Stage" workspace-status-stage)
     ("S" "Stage all" workspace-status-stage-all)
     ("u" "Fallback" workspace-status-fallback)
     ("U" "Force to Inbox" workspace-status-force-to-inbox)
     ("b" "Toggle BLOCK" workspace-status-toggle-block)
     ("k" "Archive/Delete" workspace-status-kill)
     ("v" "Tags view" workspace-status-view-proj-tag)]
    ["Project"
     ("a" "Set Active" workspace-status-project-set-active)
     ("m" "Set Maint" workspace-status-project-set-maint)
     ("f" "Set Finished" workspace-status-project-set-finished)
     ("B" "Toggle Blocked" workspace-status-project-toggle-blocked)
     ("x" "Show blockers" workspace-status-project-show-blockers)
     ("C" "Toggle auto scope" workspace-status-toggle-auto-blocked-scope)]
    ["Create"
     ("p" "Create project" workspace-status-create-project-home)]))

(defun workspace-status-help ()
  "Show Magit-like command palette."
  (interactive)
  (if workspace-status--has-transient
      (workspace-status-dispatch)
    (message "Keys: g refresh, TAB toggle/open, RET visit, s stage, u fallback, b BLOCK, k delete, v tags, C scope")))

;;; ---------- Mode ----------

(defvar workspace-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "g") #'workspace-status-refresh)
    (define-key map (kbd "?") #'workspace-status-help)
    (define-key map (kbd "TAB") #'workspace-status-tab)
    (define-key map (kbd "<tab>") #'workspace-status-tab)
    (define-key map (kbd "RET") #'workspace-status-visit-entry)
    (define-key map (kbd "s") #'workspace-status-stage)
    (define-key map (kbd "S") #'workspace-status-stage-all)
    (define-key map (kbd "u") #'workspace-status-fallback)
    (define-key map (kbd "U") #'workspace-status-force-to-inbox)
    (define-key map (kbd "b") #'workspace-status-toggle-block)
    (define-key map (kbd "n") #'workspace-status-set-next)
    (define-key map (kbd "t") #'workspace-status-set-todo)
    (define-key map (kbd "k") #'workspace-status-kill)
    (define-key map (kbd "a") #'workspace-status-project-set-active)
    (define-key map (kbd "m") #'workspace-status-project-set-maint)
    (define-key map (kbd "f") #'workspace-status-project-set-finished)
    (define-key map (kbd "B") #'workspace-status-project-toggle-blocked)
    (define-key map (kbd "x") #'workspace-status-project-show-blockers)
    (define-key map (kbd "v") #'workspace-status-view-proj-tag)
    (define-key map (kbd "C") #'workspace-status-toggle-auto-blocked-scope)
    (define-key map (kbd "N") #'workspace-status-add-item)
    (define-key map (kbd "p") #'workspace-status-create-project-from-missing)
    (define-key map (kbd "z") #'workspace-status-section-toggle)
    (define-key map (kbd "Z") #'workspace-status-sections-toggle-all)
    map)
  "Keymap for `workspace-status-mode'.")

(define-derived-mode workspace-status-mode magit-section-mode "WS-Status"
  "Workspace Status (Magit-style) for Org."
  (setq buffer-read-only t)
  (setq truncate-lines t))

;;;###autoload
(defun workspace-status ()
  "Open Workspace Status buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Workspace Status*")))
    (with-current-buffer buf
      (workspace-status-mode)
      (workspace-status-refresh))
    (pop-to-buffer buf)))

(provide 'workspace-status)

;;; workspace-status.el ends here
