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
    eshell-outline org-ai dashboard centaur-tabs all-the-icons clang-format
    blacken consult-projectile)) ; TODO: evil-textobj-tree-sitter ts-fold

(unless package-archive-contents
  (package-refresh-contents))

;; Install packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (ignore-errors (package-install package))))

;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)	 ; run manually if needed
(message "All packages installed.")

;(add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)
;(package-refresh-contents))
;(package-initialize)

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
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
;;(line-number-mode 1)
(setq scroll-step 2 scroll-conservatively 3)
;; Always highlight source code
(global-font-lock-mode 1)

(blink-cursor-mode -1) ; make cursor not blink
(setq byte-compile-warnings '(cl-functions))
(setenv "JAVA_HOME" "/opt/jdk-17.0.2")

(setq c-basic-offset 8)
(setq-default indent-tabs-mode t)

(defun untabify-buffer ()
  "Replace all spaces with tabs in the current buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (tabify (point-min) (point-max)))

(global-set-key (kbd "C-c t") 'untabify-buffer)

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

;; hiding & showing linenumber

(define-key global-map (kbd "C-w s l") 'turn-on-line-numbers-display)
(define-key global-map (kbd "C-w h l") 'turn-off-line-numbers-display)

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

(define-key global-map [(meta p)] 'windmove-up)
(define-key global-map [(meta n)] 'windmove-down)
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
  "반 페이지 아래로 스크롤하고 중앙 정렬."
  (interactive)
  (let* ((h (window-body-height))
         (lines (max 1 (/ h 2))))
    (forward-line lines)
    (recenter)))

(defun scroll-half-page-up-center ()
  "반 페이지 위로 스크롤하고 중앙 정렬."
  (interactive)
  (let* ((h (window-body-height))
         (lines (max 1 (/ h 2))))
    (forward-line (- lines))
    (recenter)))

;; Emacs 기본 키를 유지하면서 반페이지 스크롤만 재매핑
(global-set-key (kbd "C-v") #'scroll-half-page-down-center)
(global-set-key (kbd "M-v") #'scroll-half-page-up-center)

;;;; GTAGS

(defun my/generate-tags ()
  "현재 디렉터리(또는 선택한 디렉터리)에서 gtags만 생성."
  (interactive)
  (let ((default-directory (read-directory-name "Gtags 생성할 디렉터리: ")))
    (shell-command "gtags --gtagslabel=ctags")
    (message "✅ GTAGS 생성 완료: %s" (expand-file-name "GTAGS" default-directory))))
(global-set-key (kbd "C-c t") #'my/generate-tags)

(dolist (cmd '(ggtags-find-tag-dwim
               ggtags-find-tag-other-window
               xref-find-definitions))
  (advice-add cmd :after (lambda (&rest _) (recenter))))

(defun my/helm-grep-do-git-grep-at-gtags-root ()
  "GTAGS 파일이 있으면 그 디렉터리에서 helm-grep-do-git-grep 실행.
없으면 기본 Git 루트에서 실행."
  (interactive)
  (require 'ggtags nil t)
  (let* ((gtags-root
          (or (locate-dominating-file default-directory "GTAGS")
              (locate-dominating-file default-directory ".git")
              default-directory)))
    (let ((default-directory gtags-root))
      (message "🔍 helm-grep-do-git-grep @ %s" default-directory)
      (call-interactively #'helm-grep-do-git-grep))))

(global-set-key (kbd "C-c M-]") #'my/helm-grep-do-git-grep-at-gtags-root)

(defun my/ggtags-grep-auto ()
  "현재 커서 심볼로 ggtags-grep 실행 (묻지 않음)."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (when symbol
      (ggtags-grep symbol))))
(global-set-key (kbd "C-c M-.") #'my/ggtags-grep-auto)

(defun my/ggtags-find-reference-auto ()
  "현재 커서 심볼로 `ggtags-find-reference` 실행 (묻지 않음)."
  (interactive)
  (let ((symbol (thing-at-point 'symbol t)))
    (if symbol
        (ggtags-find-reference symbol)
      (call-interactively #'ggtags-find-reference))))  ;; 커서 아래 심볼 없을 때만 직접 입력
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

;;;; ---------------------------------------------------------------------------
;;;; Tree-sitter (EARLY)
;;;;   - Ensure ts grammars are found and mode remaps apply before opening files
;;;; ---------------------------------------------------------------------------

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

;;;; ---- tree-sitter(C/C++) 자동 설치 + c-mode -> c-ts-mode 리맵 ----
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
		;; Emacs가 git/cc 등을 호출해 빌드 + 설치합니다.
		(treesit-install-language-grammar lang)
		(message "[treesit] Installed grammar: %s" lang))
	    (error
	     (message "[treesit] Failed to install %s: %s" lang err))))))))

;;;; 시작할 때 한 번 확인하고 없으면 설치 시도
(add-hook 'emacs-startup-hook #'jungmo/treesit-ensure-grammars)

;; Where your compiled grammars live (libtree-sitter-*.so)
(add-to-list 'treesit-extra-load-path (expand-file-name "~/.emacs.d/tree-sitter/"))

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

(when (executable-find "xclip")
  (setq interprogram-cut-function
	(lambda (text &optional push)
	  (with-temp-buffer
	    (insert text)
	    (call-process-region (point-min) (point-max) "xclip" nil 0 nil "-selection" "clipboard"))))

  (setq interprogram-paste-function
	(lambda ()
	  (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
	    (unless (string= xclip-output "")
	      xclip-output)))))

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
;;;; Packages (use-package) — refactored layout
;;;;   - Sections grouped by purpose
;;;;   - Duplicates removed (kept the effective block)
;;;; ---------------------------------------------------------------------------

;;;; UI / Theme
(use-package color-theme-modern
  :defer t)
(load-theme 'goldenrod t t)
(enable-theme 'goldenrod)

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
    (setq ggtags-enable-navigation-keys nil))



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

(use-package ai-code-interface
  :vc (:url "https://github.com/tninja/ai-code-interface.el" :rev :newest)
  :demand t
  :config
  (require 'ai-code-codex-cli)
  (ai-code-set-backend 'codex) ;; default to OpenAI Codex CLI backend
  ;; Enable global keybinding for the main menu
  (global-set-key (kbd "C-c a") #'ai-code-menu)
  ;; Optional: Use vterm if you prefer, by default it is eat
  ;; (setq claude-code-terminal-backend 'vterm) ;; for openai codex, github copilot cli, opencode; for claude-code-ide.el and gemini-cli.el, you can check their config
  ;; Optional: Turn on auto-revert buffer, so that the AI code change automatically appears in the buffer
  (global-auto-revert-mode 1)
  (setq auto-revert-interval 1) ;; set to 1 second for faster update
  ;; Optional: Set up Magit integration for AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))


;;;; Misc / Formats
(use-package graphviz-dot-mode
  :mode ("\\.dot\\'" . graphviz-dot-mode))
;; graphviz-dot-mode 설치돼 있다고 가정
(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.gv\\'"  . graphviz-dot-mode))

(use-package bitbake-modes
  :defer t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((ai-code-interface :url
			"https://github.com/tninja/ai-code-interface.el"))))

 (with-eval-after-load 'ai-code-git
    (with-eval-after-load 'magit
      (transient-replace-suffix 'magit-log "A"	 ;; replace existing AI Code entry
	'("A" "AI Code: Analyze log" ai-code-magit-log-analyze))
      ;; If you want a different key, change "A" in both places
      ;; e.g. "z" -> '("z" "AI Code: Analyze log" ai-code-magit-log-analyze)
      ))

(add-hook 'emacs-startup-hook #'jacob/gptel-dashboard-startup)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
