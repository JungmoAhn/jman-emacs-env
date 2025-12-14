;;################################ Host Settings for MAC ################################
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'meta)
(setenv "LANG" "en_US.UTF8")
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;################################ UI Settings ################################
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
;;(line-number-mode 1)
(setq scroll-step 2)
(setq scroll-conservatively 3)
(setq global-font-lock-mode t) ;always hightlight source code
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

;;################################ Package Installing ################################
(require 'package)
(setq package-archives
      '(("gnu"       . "https://elpa.gnu.org/packages/")
	("nongnu"    . "https://elpa.nongnu.org/nongnu/")
        ("melpa"     . "https://melpa.org/packages/")
        ("onpa"      . "https://olanilsson.bitbucket.io/packages/")
        ("gnu-devel" . "https://elpa.gnu.org/devel/")
	("jcs-elpa"  . "https://jcs-emacs.github.io/jcs-elpa/packages/")
        ("onpa"      . "https://olanilsson.bitbucket.io/packages/")))  ;for bitbake

;(add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)
;(package-refresh-contents))
;(package-initialize)

;; List of packages you want to install
(defvar my-packages
  '(codegpt chatgpt orderless marginalia vertico rainbow-mode winum rustic hydra
    lsp-mode xcscope dash yasnippet which-key pyvenv projectile magit lsp-ui
    lsp-java lsp-ivy helm helm-xref helm-lsp helm-cscope helm-gtags flycheck company
    color-theme-modern elogcat bitbake-modes treesit-langs treesit-auto codex-cli gptel
    codex-theme vterm vterm-toggle vterm-hotkey eshell-git-prompt eshell-toggle
    eshell-outline org-ai dashboard centaur-tabs all-the-icons clang-format
    blacken)) ; TODO: evil-textobj-tree-sitter ts-fold

;; Install packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
(message "All packages installed.")

;;################################ Mode Settings ################################
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/src/linux-trees")
                                       filename))
                (setq indent-tabs-mode t)
                (c-set-style "linux-tabs-only")))))

(when (display-graphic-p)
  (setq select-enable-clipboard t)
  (setq select-enable-primary t))

;; X clipboard와 연동 (xclip 사용)
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
(use-package color-theme-modern)
(load-theme 'goldenrod t t)
(enable-theme 'goldenrod)

(use-package xcscope)
(use-package helm-xref)
(use-package helm-cscope)
(use-package helm
  :ensure t
  :config
  (helm-mode))
(use-package helm-gtags
  :ensure t)
(use-package gptel
  :ensure t)
;;(require 'linum-relative)

;; source contol settings
(use-package magit)
(global-set-key "\C-xg" 'magit-status)

(use-package rainbow-mode
  :ensure t)

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save-list/" t))
      backup-directory-alist
      '(("." . "~/.emacs.d/backups")))

(use-package winum
  :ensure t
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

;; tree-sitter

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

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)))

(defface treesit-face-function.call
  '((default :inherit (link font-lock-function-name-face) :underline nil :bold t :foreground "light goldenrod"))
  "Face for function calls."
  :group 'treesit-faces)

(add-hook 'eglot-managed-mode-hook (lambda ()
                                     (treesit-hl-toggle t))) ; Enable Treesitter highlighting

;; lsp-mode

;; (use-package lsp-mode
;;   :config
;;   (setq lsp-idle-delay 0.5
;;         lsp-enable-symbol-highlighting t
;;         lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
;;         lsp-pyls-plugins-flake8-enabled t)
;;   (lsp-register-custom-settings
;;    '(("pyls.plugins.pyls_mypy.enabled" t t)
;;      ("pyls.plugins.pyls_mypy.live_mode" nil t)
;;      ("pyls.plugins.pyls_black.enabled" t t)
;;      ("pyls.plugins.pyls_isort.enabled" t t)

;;      ;; Disable these as they're duplicated by flake8
;;      ("pyls.plugins.pycodestyle.enabled" nil t)
;;      ("pyls.plugins.mccabe.enabled" nil t)
;;      ("pyls.plugins.pyflakes.enabled" nil t)))
;;   :hook
;;   ((python-mode . lsp)
;;    (sh-mode . lsp)
;;    (lsp-mode . lsp-enable-which-key-integration)))

;; (use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
;;   :config (setq lsp-completion-enable-additional-text-edit nil))

;; (use-package lsp-ui
;;   :config (setq lsp-ui-sideline-show-hover t
;;                 lsp-ui-sideline-delay 0.5
;;                 lsp-ui-doc-delay 5
;;                 lsp-ui-sideline-ignore-duplicates t
;;                 lsp-ui-doc-position 'bottom
;;                 lsp-ui-doc-alignment 'frame
;;                 lsp-ui-doc-header nil
;;                 lsp-ui-doc-include-signature t
;;                 lsp-ui-doc-use-childframe t)
;;   :commands lsp-ui-mode
;;   :bind (:map evil-normal-state-map
;;               ("gd" . lsp-ui-peek-find-definitions)
;;               ("gr" . lsp-ui-peek-find-references)
;;               :map md/leader-map
;;               ("Ni" . lsp-ui-imenu)))

;; (use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
;; (use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
;; (package-install 'dap-java)
;; (use-package dap-java :ensure nil)
;; (require 'lsp-java-boot)
;; (add-hook 'lsp-mode-hook #'lsp-lens-mode)
;; (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
;; (setq-default dotspacemacs-configuration-layers
;;               '((lsp :variables lsp-lens-enable t)))

(use-package projectile)
(use-package flycheck)
(use-package yasnippet :config (yas-global-mode))
(use-package hydra)
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-gtags))
(with-eval-after-load 'company
  (setq company-backends (remove 'company-capf company-backends)))
(use-package which-key :config (which-key-mode))
(use-package helm-lsp)
;; (use-package helm
;;   :config (helm-mode))
(use-package lsp-ivy)
(use-package lsp-treemacs
  :after lsp)

;; Python

(use-package pyvenv
  :demand t
  :config
  (setq pyvenv-workon "emacs")  ; Default venv
  (pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals


;; Rustic

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

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package bitbake-modes)

(dolist (hook '(prog-mode-hook
                text-mode-hook
                c-mode-hook
                c++-mode-hook
                asm-mode-hook
                python-mode-hook
                java-mode-hook
                shell-mode-hook
                bitbake-mode-hook
                makefile-mode-hook
                makefile-gmake-mode-hook
                dts-mode-hook
                vhdl-mode-hook
                eglot-managed-mode-hook))
  (add-hook hook #'turn-on-font-lock))

;; yocto
(use-package ggtags
  :ensure t
  :hook ((c-mode c++-mode asm-mode python-mode java-mode shell-mode bitbake-mode makefile-mode makefile-gmake-mode dts-mode vhdl-mode eshell-mode) . ggtags-mode)
  :config
    (setq ggtags-enable-navigation-keys nil))
(setq ggtags-auto-update nil)

;;(add-hook 'after-save-hook #'ggtags-update-tags)
(add-to-list 'auto-mode-alist '("\\.bb\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bbclass\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . python-mode))


;;Projectile

;; (use-package projectile
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   :demand t
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :init
;;   (when (file-directory-p "~/Projects/Code")
;;     (setq projectile-project-search-path '("~/Projects/Code")))
;;   (setq projectile-switch-project-action #'projectile-dired))

;; (projectile-mode +1)
;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; (use-package counsel-projectile
;;   :after projectile)

;; (dw/leader-key-def
;;   "pf"  'counsel-projectile-find-file
;;   "ps"  'counsel-projectile-switch-project
;;   "pF"  'counsel-projectile-rg
;;   "pF"  'consult-ripgrep
;;   "pp"  'counsel-projectile
;;   "pc"  'projectile-compile-project
;;   "pd"  'projectile-dired)

;ChatGPT
(use-package codex-cli)

;(use-package chatgpt :ensure t)
;(use-package codegpt :ensure t)

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-g M-g" . consult-goto-line)
         ("C-c s" . consult-ripgrep)))

(with-eval-after-load 'consult
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

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

(defun scroll-half-page-down-center ()
  "반 페이지 아래로 스크롤하고 중앙 정렬."
  (interactive)
  (let* ((h (window-body-height))
         (lines (/ h 2)))
    (goto-char (window-start))
    (forward-line lines)
    (recenter)))

(defun scroll-half-page-up-center ()
  "반 페이지 위로 스크롤하고 중앙 정렬."
  (interactive)
  (let* ((h (window-body-height))
         (lines (/ h 2)))
    (goto-char (window-start))
    (forward-line (- lines))
    (recenter)))

(with-eval-after-load 'evil
  ;; Evil NORMAL state에서 C-v / M-v 동작 재정의
  (define-key evil-normal-state-map (kbd "C-v") #'scroll-half-page-down-center)
  (define-key evil-normal-state-map (kbd "M-v") #'scroll-half-page-up-center))

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

(global-set-key (kbd "C-c 1") #'treemacs-select-window)
(global-set-key (kbd "C-c 2") #'treemacs-switch-workspace)
(global-set-key (kbd "C-c 3") #'treemacs-edit-workspaces)
(global-set-key (kbd "C-c m") #'magit-status)
(global-set-key (kbd "C-c 0") #'magit-show-refs)
(global-set-key (kbd "C-c f") #'project-find-file)

;; M-x compile  -> create png file building dot file
(use-package graphviz-dot-mode
  :ensure t
  :mode ("\\.dot\\'" . graphviz-dot-mode))

;; graphviz-dot-mode 설치돼 있다고 가정
(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.gv\\'"  . graphviz-dot-mode))

(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

;; install required inheritenv dependency:
(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

;; for eat terminal backend:
(use-package eat :ensure t)

;; for vterm terminal backend:
(use-package vterm :ensure t)

;; install claude-code.el
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
      (transient-replace-suffix 'magit-log "A"   ;; replace existing AI Code entry
        '("A" "AI Code: Analyze log" ai-code-magit-log-analyze))
      ;; If you want a different key, change "A" in both places
      ;; e.g. "z" -> '("z" "AI Code: Analyze log" ai-code-magit-log-analyze)
      ))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
