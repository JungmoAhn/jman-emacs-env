;;################################ Package Installing ################################
(require 'package)
(setq package-archives
      '(
	("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")
	("onpa" . "https://olanilsson.bitbucket.io/packages/")
	("gnu-devel" . "https://elpa.gnu.org/devel/")
	("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
	))

;; List of packages you want to install
(defvar my-packages
  '(corfu cape
    codegpt chatgpt orderless marginalia vertico rainbow-mode winum rustic hydra
    lsp-mode xcscope dash yasnippet which-key pyvenv projectile magit lsp-ui
    lsp-java lsp-ivy helm helm-xref helm-lsp helm-cscope helm-gtags flycheck company
    color-theme-modern elogcat bitbake-modes treesit-langs treesit-auto codex-cli gptel
    codex-theme vterm vterm-toggle vterm-hotkey eshell-git-prompt eshell-toggle
    eshell-outline org-ai org-roam org-super-agenda emacsql emacsql-sqlite emacsql-sqlite-builtin sqlite3
    dashboard centaur-tabs all-the-icons clang-format multi-vterm
    blacken consult-projectile)) ; TODO: evil-textobj-tree-sitter ts-fold

;; Install packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (ignore-errors (package-install package))))

;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)	 ; run manually if needed
(message "All packages installed.")

;(add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)
(package-refresh-contents)
(package-initialize)

(setq shell-file-name "/bin/bash")
(setq explicit-shell-file-name "/bin/bash")

;;;; ---------------------------------------------------------------------------
;;;; Project (Projectile + Consult)
;;;; ---------------------------------------------------------------------------

(use-package projectile
  :init
  (projectile-mode 1)
  :custom
  ;; Use Emacs' completing-read stack (Vertico/Consult)
  (projectile-completion-system 'default)
  (projectile-enable-caching t)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package consult-projectile
  :after (projectile consult)
  :config
  (define-key projectile-command-map (kbd "f") #'consult-projectile-find-file)
  (define-key projectile-command-map (kbd "b") #'consult-projectile-switch-to-buffer)
  (define-key projectile-command-map (kbd "s") #'consult-projectile-ripgrep)
  (define-key projectile-command-map (kbd "d") #'consult-projectile-find-dir)
  (define-key projectile-command-map (kbd "p") #'consult-projectile-switch-project))

;;################################ Host Settings for MAC ################################
(setq mac-option-modifier 'meta mac-command-modifier 'meta)
(setenv "LANG" "en_US.UTF8")
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;################################ UI Settings ################################
(require 'cl-lib)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
;;(line-number-mode 1)
(setq scroll-step 2 scroll-conservatively 3)
;; Always highlight source code
(global-font-lock-mode 1)
(add-hook 'prog-mode-hook #'font-lock-mode)

(blink-cursor-mode -1) ; make cursor not blink
(setq byte-compile-warnings '(cl-functions))
(setenv "JAVA_HOME" "/opt/jdk-17.0.2")

(setq-default indent-tabs-mode t)

(setq tramp-use-ssh-controlmaster-options t)
(setq tramp-connection-timeout 30)

(defun untabify-buffer ()
  "Replace all spaces with tabs in the current buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (tabify (point-min) (point-max)))

(global-set-key (kbd "C-c t") 'untabify-buffer)

;(use-package clipetty
;  :ensure t
;  :hook (after-init . global-clipetty-mode))

(defun my--recent-file-buffers (n)
  "Return up to N most-recent buffers visiting files."
  (let (res)
    (dolist (b (buffer-list))
      (when (and (buffer-live-p b) (buffer-file-name b))
        (push b res)
        (when (>= (length res) n)
          (cl-return))))
    (nreverse res)))


;; ===== Layout helpers =====

(defun my/match-window-width (src dst)
  "Make DST window width equal to SRC window width."
  (when (and (window-live-p src) (window-live-p dst))
    (let ((delta (- (window-total-width src) (window-total-width dst))))
      (when (/= delta 0)
        (let ((ignore-window-parameters t)
              (window-min-width 1))
          (adjust-window-trailing-edge dst delta t t))))))

(defun my/set-window-width (win width)
  "Resize WIN to WIDTH columns (best-effort)."
  (when (window-live-p win)
    (let ((delta (- width (window-total-width win))))
      (when (/= delta 0)
        (let ((ignore-window-parameters t)
              (window-min-width 1))
          (adjust-window-trailing-edge win delta t t))))))

;; ===== Main layout =====
(defun my/fixed-work-layout-3col ()
  "Code | Code | Org
Shell| Shell| Org"
  (interactive)
  (delete-other-windows)

  ;; ---- Tuning knobs ----
  (let* ((org-width 40)     ;; Org column width (columns)
         (shell-lines 12))  ;; Shell height (lines)
    ;; Split forcing / narrow-frame tolerance
    (let ((split-width-threshold 0)
          (split-height-threshold nil)
          (window-min-width 10)
          (window-min-height 4))

      ;; 1) Create 3 columns: w1 | w2 | w3
      (let* ((w1 (selected-window))              ; Code/Shell col 1
             (w2 (split-window w1 nil 'right))   ; Code/Shell col 2
             (w3 (split-window w2 nil 'right)))  ; Org (full height)

        ;; 2) Split bottom shells with fixed height
        (split-window w1 (- shell-lines) 'below)
        (split-window w2 (- shell-lines) 'below)

        ;; 3) Fix Org width FIRST (so the remaining space is stable)
        (my/set-window-width w3 org-width)

        ;; 4) Make Code columns equal width (affects both top+bottom in each column)
        (my/match-window-width w1 w2)

        ;; 5) Put Org
        (select-window w3)
        (find-file "~/org/main.org")

        ;; 6) Put shells
        (select-window (window-in-direction 'below w1))
        (shell "*shell-1*")
        (select-window (window-in-direction 'below w2))
        (shell "*shell-2*")

        ;; Focus: left code
        (select-window w1)))))

;; Run on startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (ignore-errors (my/fixed-work-layout-3col))))


(winner-mode 1) ;; Restore previous window layout: C-c <left>

;;################################ Key Settings ################################
(global-unset-key "\C-w")
;;(define-key global-map (kbd "C-w r") 'windresize)
(define-key global-map [(meta l)] 'buffer-menu)
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-`") 'delete-backward-char)

(with-eval-after-load 'xref
  (define-key xref--xref-buffer-mode-map (kbd "M-p") 'xref-go-back)
  (define-key xref--xref-buffer-mode-map (kbd "M-n") 'xref-go-forward))


;; org
(define-key global-map (kbd "C-c o r") 'org-redisplay-inline-images)
(setq org-return-follows-link t)
(setq org-mouse-1-follows-link t)
(setq org-confirm-elisp-link-function nil)
(with-eval-after-load 'ol
  (unless (get 'org-link-open 'jacob/compat)
    (defvar jacob/orig-org-link-open--impl (symbol-function 'org-link-open))
    (defun org-link-open (&rest args)
      (pcase args
        (`(,link) (funcall jacob/orig-org-link-open--impl link nil))
        (`(,link ,arg) (funcall jacob/orig-org-link-open--impl link arg))
        (_ (apply jacob/orig-org-link-open--impl args))))
    (put 'org-link-open 'jacob/compat t)))
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (mouse-wheel-mode 1))

(setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "BLOCK(b)" "|" "DONE(d)" "CANCELLED(c)")))
;; hiding & showing linenumber

(define-key global-map (kbd "C-w s l") 'turn-on-line-numbers-display)
(define-key global-map (kbd "C-w h l") 'turn-off-line-numbers-display)

(global-set-key (kbd "C-M-<up>")    #'enlarge-window)
(global-set-key (kbd "C-M-<down>")  #'shrink-window)
(global-set-key (kbd "C-M-<right>") #'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<left>")  #'shrink-window-horizontally)
(repeat-mode 1)

;; hiding & showing code
;; (define-key global-map (kbd "C-w h i a") 'hide-ifdefs)
;; (define-key global-map (kbd "C-w s i a") 'show-ifdefs)
;; (define-key global-map (kbd "C-w h i b") 'hide-ifdef-block)
;; (define-key global-map (kbd "C-w s i b") 'show-ifdef-block)
;; (define-key global-map (kbd "C-w h b") 'hs-hide-block)
;; (define-key global-map (kbd "C-w s b") 'hs-show-block)
(global-set-key (kbd "C-c 1") #'treemacs-select-window)
(global-set-key (kbd "C-c 2") #'treemacs-switch-workspace)
(global-set-key (kbd "C-c 3") #'treemacs-edit-workspaces)
(global-set-key (kbd "C-c w") #'workspace-status)
(global-set-key (kbd "C-c 0") #'magit-show-refs)
(global-set-key (kbd "C-c f") #'project-find-file)

(define-key global-map (kbd "C-w p s") 'project-eshell)

(define-key global-map (kbd "C-w o") 'projectile-switch-open-project)
(define-key global-map (kbd "C-w s w") 'treemacs-switch-workspace)
(define-key global-map (kbd "C-w s p") 'treemacs-select-window)
(define-key global-map (kbd "C-w c w") 'treemacs-create-workspace)
(define-key global-map (kbd "C-w r w") 'treemacs-remove-workspace)
(define-key global-map (kbd "C-w c p") 'treemacs-peek)
(define-key global-map (kbd "C-w a p") 'treemacs-add-project-to-workspace)
(define-key global-map (kbd "C-w r p") 'treemacs-remove-project-from-workspace)
(define-key global-map (kbd "C-w n p") 'treemacs-next-project)
(define-key global-map (kbd "C-w p p") 'treemacs-previous-project)

(define-key global-map (kbd "C-w R p") 'projectil-remove-known-project)
(define-key global-map (kbd "C-w S p") 'counsel-projectile-switch-project)

;; manage a  current workspace
(define-key global-map (kbd "C-w a f") 'lsp-workspace-folders-add)
(define-key global-map (kbd "C-w r f") 'lsp-workspace-folders-remove)

;; moving cursor

(define-key global-map (kbd "C-w w") 'winum-select-window-by-number)
(define-key global-map (kbd "C-w g") 'goto-line)
(define-key global-map (kbd "C-w l") 'enlarge-window)
(define-key global-map (kbd "C-w 1") 'winum-select-window-1)
(define-key global-map (kbd "C-w 2") 'winum-select-window-2)
(define-key global-map (kbd "C-w 3") 'winum-select-window-3)
(define-key global-map (kbd "C-w 4") 'winum-select-window-4)

(define-key global-map [(meta u)] 'windmove-up)
(define-key global-map [(meta d)] 'windmove-down)
(define-key global-map [(meta f)] 'windmove-right)
(define-key global-map [(meta b)] 'windmove-left)

(define-key global-map (kbd "C-w b b") 'beginning-of-buffer)
(define-key global-map (kbd "C-w b p") 'backward-paragraph)
(define-key global-map (kbd "C-w b s") 'backward-sentence)
(define-key global-map (kbd "C-w b i") 'backward-ifdef)
(define-key global-map (kbd "C-w f e") 'end-of-buffer)
(define-key global-map (kbd "C-w f p") 'forward-paragraph)
(define-key global-map (kbd "C-w f s") 'forward-sentence)
(define-key global-map (kbd "C-w f i") 'forward-ifdef)

(define-key global-map (kbd "C-w d") 'toggle-window-dedicated)
;; (define-key global-map [f3] 'cscope-find-functions-calling-this-function)
;; (define-key global-map [f1] 'lsp-ui-peek-find-definitions)
;; (define-key global-map [f2] 'lsp-ui-peek-find-references)

;;(define-key global-map [f4] 'cscope-find-this-references)
;;(define-key global-map [f4] 'lsp-ivy-global-workspace-symbol)
(define-key global-map [f5] 'lsp-mode)
(define-key global-map [f6] 'lsp-describe-session)
;;(global-set-key [f6] 'find-file-in-repository)
;;(define-key global-map [f7] 'grep-find)
;;(define-key global-map [f8] 'project-shell)
(define-key global-map [f12] 'whitespace-cleanup)
;;(define-key global-map [f4] 'cscope-find-this-symbol)
;;commenting with DOXYMACS

;;(define-key global-map [f5] 'doxymacs-insert-file-comment)
;;(define-key global-map [f6] 'doxymacs-insert-function-comment)
;;(define-key global-map [f7] 'doxymacs-insert-blank-multiline-comment)
;;(define-key global-map [f8] 'comment-or-uncomment-region)

;;################################ magit ################################
;;(define-key global-map [(meta 9)] 'windmove-up)
;;(define-key global-map [(meta 0)] 'windmove-up)

;; pin a window
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not."
  (interactive)
  (message
   (if (let ((window (get-buffer-window (current-buffer))))
	 (set-window-dedicated-p window (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;;map key 'e' in cscope-buffer
(defun cscope-select-entry-edit1-window ()
  "Display the entry at point in a edit1 window, select the window."
  (interactive)
  (let* ((edit-win-list (ecb-canonical-edit-windows-list))
	 (edit-win (or (and (not 1)
			    (car edit-win-list))
		       (and 1
			    (> 1 0)
			    (<= 1 (length edit-win-list))
			    (nth (1- 1) edit-win-list))
		       (and (> (length edit-win-list) 1)
			    (nth 1 edit-win-list))
		       (car edit-win-list))))
    (let ((file (get-text-property (point) 'cscope-file))
	  (line-number (get-text-property (point) 'cscope-line-number)))
      (setq edit-win (cscope-show-entry-internal file line-number t edit-win))
      (when (windowp edit-win)
	(select-window edit-win)))))
;;;; SCROLL
(defun scroll-half-page-down-center ()
  "Scroll down by half a page and recenter."
  (interactive)
  (let* ((h (window-body-height))
         (lines (max 1 (/ h 2))))
    (forward-line lines)
    (recenter)))

(defun scroll-half-page-up-center ()
  "Scroll up by half a page and recenter."
  (interactive)
  (let* ((h (window-body-height))
         (lines (max 1 (/ h 2))))
    (forward-line (- lines))
    (recenter)))

;; Keep default Emacs bindings and remap only half-page scrolling
(global-set-key (kbd "C-v") #'scroll-half-page-down-center)
(global-set-key (kbd "M-v") #'scroll-half-page-up-center)

;;;; GTAGS
(defun my/generate-tags ()
  "Generate GTAGS in the current directory (or a selected directory)."
  (interactive)
  (let ((default-directory (read-directory-name "Directory for GTAGS: ")))
    (shell-command "gtags --gtagslabel=ctags")
    (message "GTAGS created: %s" (expand-file-name "GTAGS" default-directory))))
(global-set-key (kbd "C-c t") #'my/generate-tags)

(dolist (cmd '(ggtags-find-tag-dwim
               ggtags-find-tag-other-window
               xref-find-definitions))
  (advice-add cmd :after (lambda (&rest _) (recenter))))

(defun my/helm-grep-do-git-grep-at-gtags-root ()
  "Run helm-grep-do-git-grep from the GTAGS directory if present.
Otherwise, run it from the Git root."
  (interactive)
  (require 'ggtags nil t)
  (let* ((gtags-root
          (or (locate-dominating-file default-directory "GTAGS")
              (locate-dominating-file default-directory ".git")
              default-directory)))
    (let ((default-directory gtags-root))
      (message "helm-grep-do-git-grep @ %s" default-directory)
      (call-interactively #'helm-grep-do-git-grep))))

(global-set-key (kbd "C-c M-]") #'my/helm-grep-do-git-grep-at-gtags-root)

(defun my/ggtags-grep-auto ()
  "Run ggtags-grep for the symbol at point without prompting."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (when symbol
      (ggtags-grep symbol))))
(global-set-key (kbd "C-c M-.") #'my/ggtags-grep-auto)

(defun my/ggtags-find-reference-auto ()
  "Run ggtags-find-reference for the symbol at point without prompting."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (if symbol
        (ggtags-find-reference symbol)
      (call-interactively #'ggtags-find-reference))))  ;; Prompt only when there is no symbol at point
(global-set-key (kbd "C-c M-r") #'my/ggtags-find-reference-auto)

;;################################ Package Installing ################################
(require 'package)
(setq package-archives
      '(
	("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")
	("onpa" . "https://olanilsson.bitbucket.io/packages/")
	("gnu-devel" . "https://elpa.gnu.org/devel/")
	("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
	))

;(add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)
;(package-refresh-contents))
;(package-initialize)

(defun my/osc52-copy (text &optional _push)
  (let ((encoded (base64-encode-string text t)))
    (send-string-to-terminal
     (concat "\033]52;c;" encoded "\a"))))

(setq interprogram-cut-function #'my/osc52-copy)

(defun my-smart-yank ()
  (interactive)
  (if (and (boundp 'interprogram-paste-function)
           interprogram-paste-function)
      (let ((clip (ignore-errors (funcall interprogram-paste-function))))
        (if (and clip (not (string-empty-p clip)))
            (insert clip)
          (yank)))
    (yank)))

(global-set-key (kbd "C-y") #'my-smart-yank)

;;;; ---------------------------------------------------------------------------
;;;; Tree-sitter (EARLY)
;;;;   - Ensure ts grammars are found and mode remaps apply before opening files
;;;; ---------------------------------------------------------------------------
;; --- Tree-sitter grammars auto-install if ~/.emacs.d/tree-sitter is missing ---
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))


(defun jungmo/treesit-ensure-grammars (&optional langs)
  "Ensure tree-sitter grammars for LANGS are installed.
Default LANGS: '(c cpp). Safe to call multiple times."
  (let ((langs (or langs '(c cpp))))
    (when (and (fboundp 'treesit-available-p)
	       (treesit-available-p)
	       (fboundp 'treesit-language-available-p)
	       (fboundp 'treesit-install-language-grammar))
      (dolist (lang langs)
	(unless (treesit-language-available-p lang)
	  (condition-case err
	      (progn
		(message "[treesit] Installing grammar: %s" lang)
		;; Emacs runs git and a C compiler to build and install the grammar.
		(treesit-install-language-grammar lang)
		(message "[treesit] Installed grammar: %s" lang))
	    (error
	     (message "[treesit] Failed to install %s: %s" lang err))))))))

(let ((ts-dir (expand-file-name "~/.emacs.d/tree-sitter/")))
  ;; 1) Add local tree-sitter dir to Emacs tree-sitter search path (if variables exist)
  (when (file-directory-p ts-dir)
    (cond
     ((boundp 'treesit-extra-load-path) (add-to-list 'treesit-extra-load-path ts-dir))
     ((boundp 'treesit-load-path)       (add-to-list 'treesit-load-path ts-dir))))
  ;; 2) If directory is missing, create it and install grammars (only if supported)
  (unless (file-directory-p ts-dir)
    (make-directory ts-dir t)
    ;; If you use the treesit-langs package, set its directory (if variable exists)
    (when (boundp 'treesit-langs-grammar-dir)
      (setq treesit-langs-grammar-dir ts-dir))
    ;; Run install only if the function exists (avoid startup crash)
    (when (fboundp 'treesit-langs-install-grammars)
      (treesit-langs-install-grammars))))

;;;; Check once at startup and try installation if missing
(add-hook 'emacs-startup-hook #'jungmo/treesit-ensure-grammars)

;; Where your compiled grammars live (libtree-sitter-*.so)
(let ((x (expand-file-name "~/.emacs.d/tree-sitter/")))
  (when (boundp 'treesit-extra-load-path)
    (add-to-list 'treesit-extra-load-path x)))


;; sqlite3.el (from emacs-sqlite3-api)
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
;(require 'sqlite3 nil t)

(defun jungmo/treesit--remap (from to lang)
  "Add (FROM . TO) to `major-mode-remap-alist` if TO exists and LANG grammar is available."
  (when (and (fboundp 'treesit-available-p)
	     (treesit-available-p)
	     (fboundp 'treesit-language-available-p)
	     (treesit-language-available-p lang)
	     (fboundp to))
    (add-to-list 'major-mode-remap-alist (cons from to))))

(defun jungmo/treesit-remap-modes ()
  "Remap classic modes to *-ts-mode when available.

This only remaps when BOTH are true:
- the grammar is available (`treesit-language-available-p`)
- the ts major mode exists (`fboundp` check inside `jungmo/treesit--remap`)

So it's safe even if you haven't installed some *-ts-mode packages
(e.g., markdown-ts-mode, org-ts-mode, makefile-ts-mode, ...)."

  ;; Core languages (Emacs built-in ts-modes on 29/30)
  (jungmo/treesit--remap 'c-mode	  'c-ts-mode	      'c)
  (jungmo/treesit--remap 'c++-mode	  'c++-ts-mode	      'cpp)
  (jungmo/treesit--remap 'python-mode	  'python-ts-mode     'python)
  (jungmo/treesit--remap 'rust-mode	  'rust-ts-mode	      'rust)
  (jungmo/treesit--remap 'json-mode	  'json-ts-mode	      'json)
  (jungmo/treesit--remap 'sh-mode	  'bash-ts-mode	      'bash)
  (jungmo/treesit--remap 'dockerfile-mode 'dockerfile-ts-mode 'dockerfile)
  (jungmo/treesit--remap 'yaml-mode	  'yaml-ts-mode	      'yaml)
  (jungmo/treesit--remap 'toml-mode	  'toml-ts-mode	      'toml)

  ;; Web / markup (if you use these modes)
  (jungmo/treesit--remap 'css-mode	  'css-ts-mode	      'css)
  (jungmo/treesit--remap 'html-mode	  'html-ts-mode	      'html)
  (jungmo/treesit--remap 'js-mode	  'js-ts-mode	      'javascript)
  (jungmo/treesit--remap 'js2-mode	  'js-ts-mode	      'javascript)
  (jungmo/treesit--remap 'typescript-mode 'typescript-ts-mode 'typescript)
  (jungmo/treesit--remap 'tsx-ts-mode	  'tsx-ts-mode	      'tsx) ; noop if already ts-mode
  (jungmo/treesit--remap 'typescript-ts-mode 'typescript-ts-mode 'typescript) ; noop

  ;; More languages (only if you have those modes enabled)
  (jungmo/treesit--remap 'go-mode	  'go-ts-mode	      'go)
  (jungmo/treesit--remap 'java-mode	  'java-ts-mode	      'java)
  (jungmo/treesit--remap 'csharp-mode	  'csharp-ts-mode     'c-sharp)

  ;; Docs / build / misc (often require external *-ts-mode packages)
  (jungmo/treesit--remap 'makefile-mode	  'makefile-ts-mode   'make)
  (jungmo/treesit--remap 'markdown-mode	  'markdown-ts-mode   'markdown)
  (jungmo/treesit--remap 'org-mode	  'org-ts-mode	      'org)
  (jungmo/treesit--remap 'asm-mode	  'asm-ts-mode	      'asm)
  (jungmo/treesit--remap 'doxygen-mode	  'doxygen-ts-mode    'doxygen)
  (jungmo/treesit--remap 'git-commit-mode 'gitcommit-ts-mode  'gitcommit))

(defface treesit-face-function.call
  '((default :inherit (link font-lock-function-name-face) :underline nil :bold t :foreground "light goldenrod"))
  "Face for function calls."
  :group 'treesit-faces)

(add-hook 'eglot-managed-mode-hook (lambda ()
				     (treesit-hl-toggle t))) ; Enable Treesitter highlighting

;; Run immediately so `emacs file.c` starts in c-ts-mode on the first buffer.
(jungmo/treesit-remap-modes)
(add-hook 'c-ts-mode-hook (lambda () (font-lock-mode 1))) ;; For Emacs 29+ tree-sitter

;; List of packages you want to install
(defvar my-packages
  '(corfu cape
    codegpt chatgpt orderless marginalia vertico rainbow-mode winum rustic hydra
    lsp-mode xcscope dash yasnippet which-key pyvenv projectile magit lsp-ui
    lsp-java lsp-ivy helm helm-xref helm-lsp helm-cscope helm-gtags flycheck company
    color-theme-modern elogcat bitbake-modes treesit-langs treesit-auto codex-cli gptel
    codex-theme vterm vterm-toggle vterm-hotkey eshell-git-prompt eshell-toggle
    eshell-outline org-ai dashboard centaur-tabs all-the-icons clang-format
    blacken)) ; TODO: evil-textobj-tree-sitter ts-fold

;; Install packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (ignore-errors (package-install package))))

;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)	 ; run manually if needed
(message "All packages installed.")

;;################################ Mode Settings ################################
;; Linux / kernel-ish defaults
(setq-default tab-width 8)

(with-eval-after-load 'cc-mode
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (c-add-style
	       "linux-tabs-only"
	       '("linux"
		 (c-offsets-alist
		  (arglist-cont-nonempty
		   c-lineup-gcc-asm-reg
		   c-lineup-arglist-tabs-only))))

	      (c-set-style "linux-tabs-only")

	      (setq indent-tabs-mode t)
	      (setq c-indent-tabs-mode t)
	      (setq tab-width 8)
	      (setq c-basic-offset 8))))

(with-eval-after-load 'c-ts-mode
  (add-hook 'c-ts-mode-hook
	    (lambda ()
	      (setq-local indent-tabs-mode t)
	      (setq-local tab-width 8)
	      (setq-local c-ts-mode-indent-offset 8)))
  (add-hook 'c++-ts-mode-hook
	    (lambda ()
	      (setq-local indent-tabs-mode t)
	      (setq-local tab-width 8)
	      (setq-local c-ts-mode-indent-offset 8))))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(when (display-graphic-p)
  (setq select-enable-clipboard t)
  (setq select-enable-primary t))

;;hideshow for programming
(load-library "hideshow")
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;;hide-ifdef
(add-hook 'c-mode-common-hook 'hide-ifdef-mode)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(add-hook 'java-mode-hook 'eglot-ensure)

(add-hook 'python-mode-hook 'eglot-ensure)
;(add-hook 'python-mode-hook 'python-ts-mode)

;;(add-hook 'c-mode-hook 'lsp)
;;(add-hook 'cpp-mode-hook 'lsp)
;;(add-hook 'python-mode-hook 'lsp)
;;(add-hook 'java-mode-hook 'lsp)

;; LSP Settings
;;TODO: M-x lsp-install-server

(defun my-term-scroll-to-bottom ()
  "Keep cursor at the bottom in term/serial-term buffers."
  (when (derived-mode-p 'term-mode)
    (goto-char (point-max))))

(add-hook 'term-output-hook 'my-term-scroll-to-bottom)

;################################ Package Settings ################################


;;;; ---------------------------------------------------------------------------
;;;; Completion (Modern Emacs stack)
;;;;   Minibuffer: Vertico + Orderless + Marginalia + Consult + Embark
;;;;   In-buffer:  Corfu + Cape
;;;; ---------------------------------------------------------------------------

;; Persistent minibuffer history
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

;; Recent files
(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 200
	recentf-max-menu-items 50)
  (recentf-mode 1))

;; Vertico: vertical completion UI for `completing-read`
(use-package vertico
  :init
  (vertico-mode 1)
  ;; Optional: keep the UI snappy
  (setq vertico-cycle t))

;; Orderless: flexible matching style
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	;; Make file completion a bit more conservative
	completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia: richer annotations in minibuffer
(use-package marginalia
  :init
  (marginalia-mode 1))

;; Consult: powerful search/navigation commands using `completing-read`
(use-package consult
  :bind (("C-s"	  . consult-line)
	 ("C-c h" . consult-history)
	 ("C-c m" . consult-mode-command)
	 ("C-c k" . consult-kmacro)
	 ("C-x b" . consult-buffer)
	 ("M-y"	  . consult-yank-pop)
	 ("C-x C-r" . consult-recent-file))
  :init
  ;; Optional: preview key (works nicely with Vertico)
  (setq consult-preview-key "M-."))

;; Embark: context actions
(use-package embark
  :bind (("C-." . embark-act)
	 ("C-;" . embark-dwim)
	 ("C-h B" . embark-bindings))
  :init
  ;; Replace the default prefix help with an Embark-powered one
  (setq prefix-help-command #'embark-prefix-help-command))

;; Show Embark collections with Consult UI
(use-package embark-consult
  :after (embark consult))

;; Corfu: in-buffer completion UI (CAPF)
(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 2)
  (corfu-preselect 'prompt)
  :bind (:map corfu-map
	      ("M-SPC" . corfu-insert-separator)
	      ("TAB"   . corfu-next)
	      ([tab]   . corfu-next)
	      ("S-TAB" . corfu-previous)
	      ([backtab] . corfu-previous)))

;; Cape: extra completion-at-point sources
(use-package cape
  :init
  ;; Add useful completion sources. Keep them light.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol))

;; Optional: Edit grep results directly (wgrep)
(use-package wgrep
  :defer t)


;; use-package baseline (clean + consistent)
(require 'package)
(setq package-enable-at-startup nil)
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

;;;; ---------------------------------------------------------------------------
;;;; Packages (use-package) - refactored layout
;;;;   - Sections grouped by purpose
;;;; Packages (use-package) - refactored layout
;;;; ---------------------------------------------------------------------------
(setq custom-safe-themes t)
(add-hook 'after-init-hook
          (lambda ()
            (load-theme 'goldenrod t)))
;;(use-package color-theme-modern
;;  :defer t)
(load-theme 'goldenrod t)


(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

(use-package rainbow-mode
  :ensure t)

(use-package winum
  :config
  (global-set-key (kbd "M-0") 'treemcas-select-window)
  (global-set-key (kbd "M-1") 'winum-select-window-1)
  (global-set-key (kbd "M-2") 'winum-select-window-2)
  (global-set-key (kbd "M-3") 'winum-select-window-3)
  (global-set-key (kbd "M-4") 'winum-select-window-4)
  (global-set-key (kbd "M-5") 'winum-select-window-5)
  (global-set-key (kbd "M-6") 'winum-select-window-6)
  (global-set-key (kbd "M-7") 'winum-select-window-7)
  (global-set-key (kbd "M-8") 'winum-select-window-8)
  (winum-mode))


;;;; Minibuffer / Completion
(use-package vertico
  :config
  (vertico-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless)))

(use-package consult
  :bind (("C-s" . consult-line)
	 ("M-g g" . consult-goto-line)
	 ("M-g i" . consult-imenu)
	 ("M-g M-g" . consult-goto-line)
	 ("C-c s" . consult-ripgrep)))

(use-package which-key :config (which-key-mode))



(use-package yasnippet :config (yas-global-mode))

(use-package hydra
  :defer t)


;;;; Navigation / Tags








(use-package xcscope
  :defer t)

(use-package ggtags
  :hook ((c-mode c++-mode asm-mode python-mode java-mode shell-mode bitbake-mode makefile-mode makefile-gmake-mode dts-mode vhdl-mode eshell-mode) . ggtags-mode)
  :config
    (setq ggtags-enable-navigation-keys nil)
    (define-key ggtags-mode-map (kbd "C-]") #'ggtags-find-tag-dwim)
    (define-key ggtags-mode-map (kbd "M-.") #'xref-find-definitions)
)



(use-package lsp-ivy
  :defer t)





;;;; ---------------------------------------------------------------------------
;;;; LSP (Eglot)
;;;; ---------------------------------------------------------------------------

(use-package eglot
  :defer t
  :hook ((python-mode . eglot-ensure)
	 (c-mode      . eglot-ensure)
	 (c-ts-mode   . eglot-ensure)
	 (c++-mode    . eglot-ensure)
	 (c++-ts-mode . eglot-ensure)
	 (rust-mode   . eglot-ensure)
	 (rust-ts-mode . eglot-ensure))
  :custom
  ;; Optional quality-of-life tweaks
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.5)
  ;; Disable on-type formatting globally (prevents tabs->spaces on RET).
  ;; you can enable it if you use .clangd for clangd format
  (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
  :config
  ;; Prefer tree-sitter highlighting when available
  (when (and (fboundp 'treesit-hl-toggle)
	     (fboundp 'treesit-available-p)
	     (treesit-available-p))
    (add-hook 'eglot-managed-mode-hook (lambda () (treesit-hl-toggle 1)))))

;;;; LSP / DAP
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-java :ensure nil)

(use-package flycheck
  :defer t)

(use-package pyvenv
  :demand t
  :config
  (setq pyvenv-workon "emacs")	; Default venv
  (pyvenv-tracking-mode 1))

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
	      ("M-j" . lsp-ui-imenu)
	      ("M-?" . lsp-find-references)
	      ("C-c C-c l" . flycheck-list-errors)
	      ("C-c C-c a" . lsp-execute-code-action)
	      ("C-c C-c r" . lsp-rename)
	      ("C-c C-c q" . lsp-workspace-restart)
	      ("C-c C-c Q" . lsp-workspace-shutdown)
	      ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))


;;;; Git
(use-package magit
  :defer t)


;;;; Terminals / Shell
(use-package vterm :ensure t)
;;#'vterm-copy-mode))

(use-package eat :ensure t)

(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))


;;;; AI / Chat
(use-package gptel
  :ensure t)

(use-package codex-cli
  :defer t)

(use-package chatgpt :ensure t)

(use-package codegpt :ensure t)

(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest))

(defun revert-all-file-buffers ()
  "Revert all file buffers without confirmation."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name buf)
                 (file-exists-p (buffer-file-name buf)))
        (revert-buffer :ignore-auto :noconfirm))))
  (message "All file buffers reverted."))

(global-set-key (kbd "C-c r") 'revert-all-file-buffers)

;;;; Misc / Formats
(use-package graphviz-dot-mode
  :mode ("\\.dot\\'" . graphviz-dot-mode))
;; Assume graphviz-dot-mode is installed
(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.gv\\'"  . graphviz-dot-mode))

(use-package bitbake-modes
  :defer t)

(use-package ai-code
  :config
  ;; use codex as backend, other options are 'gemini, 'github-copilot-cli, 'opencode, 'grok, 'claude-code-ide, 'claude-code-el, 'claude-code, 'cursor, 'kiro, 'codebuddy, 'aider, 'agent-shell
  (ai-code-set-backend 'codex) ;; set your preferred backend
  (global-set-key (kbd "C-c a") #'ai-code-menu)
  ;; Optional: Enable @ file completion in comments and AI sessions
  (ai-code-prompt-filepath-completion-mode 1)
  ;; Optional: Configure AI test prompting mode (e.g., ask about running tests/TDD) for a tighter build-test loop
  (setq ai-code-auto-test-type 'ask-me)
  ;; Optional: In the AI session buffer (Evil normal state), SPC triggers the prompt entry UI
  (with-eval-after-load 'evil (ai-code-backends-infra-evil-setup))
  (global-auto-revert-mode 1)
  (setq auto-revert-interval 1) ;; set to 1 second for faster update
  )

 (with-eval-after-load 'ai-code-git
    (with-eval-after-load 'magit
      (transient-replace-suffix 'magit-log "A"	 ;; replace existing AI Code entry
	'("A" "AI Code: Analyze log" ai-code-magit-log-analyze))
      ;; If you want a different key, change "A" in both places
      ;; e.g. "z" -> '("z" "AI Code: Analyze log" ai-code-magit-log-analyze)
      ))

;;; pr automation
;(defvar jacob/jenkins-base-url "https://jenkins.sample.local")
;(defvar jacob/lava-base-url    "https://lava.sample.local")
;(load "/path/to/pr-ci-hil-automation.el")

;;; emacs notion
;(require 'org-triage-ai)
;(load "~/.emacs.d/lisp/org-workspace-suite.el")
(load "~/.emacs.d/lisp/enotion/workspace-status.el")
(load "~/.emacs.d/lisp/enotion/treemacs-workflows.el")

(load-theme 'goldenrod t)

(when (fboundp 'jacob/gptel-dashboard-startup)
  (add-hook 'emacs-startup-hook #'jacob/gptel-dashboard-startup))
