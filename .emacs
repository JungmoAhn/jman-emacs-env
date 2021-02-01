;; mac settings
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'meta)
(setenv "LANG" "en_US.UTF8")
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; UI settings
(menu-bar-mode 0)
(tool-bar-mode 0)
;(line-number-mode 1)
(setq scroll-step 2)
(setq scroll-conservatively 3)
(global-font-lock-mode t) ;always hightlight source code
(blink-cursor-mode -1) ; make cursor not blink

;;; emacs package settings
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(package-refresh-contents)
(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))
; make more packages available with the package installer
(setq to-install
      '(python-mode magit linum-relative epc virtualenv exec-path-from-shell pydoc anaconda-mode color-theme-modern lsp-mode yasnippet lsp-treemacs helm-lsp lsp-mode yasnippet lsp-treemacs helm-lsp projectile hydra flycheck company company-box  avy which-key helm-xref dap-mode package lsp-ivy counsel-projectile lsp-ui helm-cscope lsp-python-ms pyvenv))

(setq byte-compile-warnings '(cl-functions))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(mapc 'install-if-needed to-install)
(require 'package)
(package-initialize)
(package-refresh-contents)
(package-install 'use-package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.50")
 '(indent-line-function (quote insert-tab) t)
 '(indent-tabs-mode t)
 '(package-selected-packages
   (quote
    (elpy bitbake color-theme-modern ein anaconda-mode pydoc exec-path-from-shell virtualenv linum-relative yasnippet helm-cscope python-mode magit jedi flycheck find-file-in-repository ecb autopair)))
 '(tab-width 8))
(add-hook 'text-mode-hook
      (lambda() (setq indent-line-function 'insert-tab)))
(setq c-basic-offset 8)

;(setq-default indent-tabs-mode nil); user 8 spaces instead of 1 tab
;(setq c-basic-offset 9)
;(setq term-buffer-maximumsize 0)

;;Key settings
(global-unset-key "\C-w")
(define-key global-map (kbd "C-w r") 'windresize)
(define-key global-map [(meta l)] 'buffer-menu)
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-`") 'delete-backward-char)

;; org
(define-key global-map (kbd "C-c o r") 'org-redisplay-inline-images)

;; hiding & showing linenumber

(define-key global-map (kbd "C-w s l") 'turn-on-line-numbers-display)
(define-key global-map (kbd "C-w h l") 'turn-off-line-numbers-display)

;; hiding & showing code
(define-key global-map (kbd "C-w h i a") 'hide-ifdefs)
(define-key global-map (kbd "C-w s i a") 'show-ifdefs)
(define-key global-map (kbd "C-w h i b") 'hide-ifdef-block)
(define-key global-map (kbd "C-w s i b") 'show-ifdef-block)
(define-key global-map (kbd "C-w h b") 'hs-hide-block)
(define-key global-map (kbd "C-w s b") 'hs-show-block)

;; moving cursor

(define-key global-map [(meta p)] 'windmove-up)
(define-key global-map [(meta n)] 'windmove-down)
(define-key global-map [(meta f)] 'windmove-right)
(define-key global-map [(meta b)] 'windmove-left)
(define-key global-map (kbd "C-w g") 'goto-line)
(define-key global-map (kbd "C-w b b") 'beginning-of-buffer)
(define-key global-map (kbd "C-w b p") 'backward-paragraph)
(define-key global-map (kbd "C-w b s") 'backward-sentence)
(define-key global-map (kbd "C-w b i") 'backward-ifdef)
(define-key global-map (kbd "C-w f e") 'end-of-buffer)
(define-key global-map (kbd "C-w f p") 'forward-paragraph)
(define-key global-map (kbd "C-w f s") 'forward-sentence)
(define-key global-map (kbd "C-w f i") 'forward-ifdef)

;; pin a window

(defun toggle-window-dedicated ();; Toggle window dedication
"Toggle whether the current active window is dedicated or not"

(interactive)

(message 

 (if (let (window (get-buffer-window (current-buffer)))

       (set-window-dedicated-p window 

        (not (window-dedicated-p window))))

    "Window '%s' is dedicated"

    "Window '%s' is normal")

 (current-buffer)))

(define-key global-map (kbd "C-w d") 'toggle-window-dedicated)
;(define-key global-map [f3] 'cscope-find-functions-calling-this-function)
(define-key global-map [f1] 'treemacs-select-window)
(define-key global-map [f2] 'lsp-ivy-global-workspace-symbol)
(define-key global-map [f3] 'helm-cscope-find-this-symbol)
;(define-key global-map [f4] 'cscope-find-this-symbol)
(global-set-key [f7] 'find-file-in-repository)
(define-key global-map "\C-]" 'helm-cscope-find-global-definition)
(define-key global-map "\C-t" 'helm-cscope-pop-mark)
;(define-key global-map "\C-]" 'xref-find-definitions)
;(define-key global-map "\C-t" 'xref-find-references)

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
    (if (windowp edit-win)
	  (select-window edit-win))
    ))
)

;;commenting with DOXYMACS

(define-key global-map [f5] 'doxymacs-insert-file-comment)
(define-key global-map [f6] 'doxymacs-insert-function-comment)
(define-key global-map [f7] 'doxymacs-insert-blank-multiline-comment)
;(define-key global-map [f8] 'comment-or-uncomment-region)
(define-key global-map [f8] 'whitespace-cleanup)

;;magit

(define-key global-map [f9] 'magit-status)
(define-key global-map [f10] 'magit-show-refs)
(define-key global-map [f11] 'magit-log)
(define-key global-map [f12] 'magit-remote-config-popup)

;(define-key global-map [(meta 9)] 'windmove-up)
;(define-key global-map [(meta 0)] 'windmove-up)
(define-key global-map (kbd "C-9") 'magit-status)
(define-key global-map (kbd "C-0") 'magit-show-refs)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((((type tty)) (:foreground "green"))))
 '(magit-diff-added-highlight ((((type tty)) (:foreground "LimeGreen"))))
 '(magit-diff-context-highlight ((((type tty)) (:foreground "default"))))
 '(magit-diff-file-heading ((((type tty)) nil)))
 '(magit-diff-removed ((((type tty)) (:foreground "red"))))
 '(magit-diff-removed-highlight ((((type tty)) (:foreground "IndianRed"))))
 '(magit-section-highlight ((((type tty)) nil))))

;; show magit-status in current window 
;;(setq magit-status-buffer-switch-function 'switch-to-buffer)

;;hideshow for programming
(load-library "hideshow")
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;;hide-ifdef
(add-hook 'c-mode-common-hook 'hide-ifdef-mode)                

;; require packages & settings
;;(setq color-theme-is-global t)

;;(color-theme-goldenrod)
;;(color-theme-dark-blue2)
;;(color-theme-gray30)

;;(color-theme-modern
(load-theme 'goldenrod t t)
(enable-theme 'goldenrod)

(require 'xcscope)
;(require 'linum-relative)

;; source contol settings
(require 'magit)
(global-set-key "\C-xg" 'magit-status)

(require 'auto-complete)
; auto-complete mode extra settings
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 20)

(require 'autopair)

;; C language settings
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

;; LSP Settings

(add-hook 'c-mode-hook 'lsp)
(add-hook 'cpp-mode-hook 'lsp)
(add-hook 'python-mode-hook 'lsp)

(use-package lsp-mode
  :config
  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
        lsp-pyls-plugins-flake8-enabled t)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)

     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  :hook
  ((python-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :config (setq lsp-ui-sideline-show-hover t
                lsp-ui-sideline-delay 0.5
                lsp-ui-doc-delay 5
                lsp-ui-sideline-ignore-duplicates t
                lsp-ui-doc-position 'bottom
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-header nil
                lsp-ui-doc-include-signature t
                lsp-ui-doc-use-childframe t)
  :commands lsp-ui-mode
  :bind (:map evil-normal-state-map
              ("gd" . lsp-ui-peek-find-definitions)
              ("gr" . lsp-ui-peek-find-references)
              :map md/leader-map
              ("Ni" . lsp-ui-imenu)))

(use-package pyvenv
  :demand t
  :config
  (setq pyvenv-workon "emacs")  ; Default venv
  (pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package lsp-ivy)
(ivy-mode 1)
(use-package lsp-treemacs
  :after lsp)

;;(use-package counsel-projectile
;;  :after projectile)

;(dw/leader-key-def
;  "pf"  'counsel-projectile-find-file
;  "ps"  'counsel-projectile-switch-project
;  "pF"  'counsel-projectile-rg
;  ;; "pF"  'consult-ripgrep
;  "pp"  'counsel-projectile
;  "pc"  'projectile-compile-project
;  "pd"  'projectile-dired)
