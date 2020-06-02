;; emacs package settings
(require 'package)
(package-initialize)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-refresh-contents)
(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)))

; make more packages available with the package installer
(setq to-install
      '(python-mode magit yasnippet jedi auto-complete autopair find-file-in-repository flycheck xcscope ecb linum-relative epc virtualenv exec-path-from-shell pydoc anaconda-mode ein))

(mapc 'install-if-needed to-install)

;; mac settings
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'meta)
(setenv "LANG" "en_US.UTF8")
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(add-hook 'python-mode-hook
  (lambda ()
        (set (make-variable-buffer-local 'beginning-of-defun-function)
             'py-beginning-of-def-or-class)
        (define-key py-mode-map "\C-c\C-z" 'py-shell)
        (setq outline-regexp "def\\|class ")
        (setenv "LANG" "en_GB.UTF-8"))) ; <-- *this* line is new

(menu-bar-mode 0)
(tool-bar-mode 0)
;(line-number-mode 1)
(setq scroll-step 2)
(setq scroll-conservatively 3)
(global-font-lock-mode t) ;always hightlight source code
(blink-cursor-mode -1) ; make cursor not blink

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
    (bitbake color-theme-modern ein anaconda-mode pydoc exec-path-from-shell virtualenv linum-relative yasnippet xcscope python-mode magit jedi flycheck find-file-in-repository ecb autopair)))
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

;;ECB

(define-key global-map (kbd "C-w 0") 'ecb-goto-window-directories)
(define-key global-map (kbd "C-w 1") 'ecb-goto-window-sources)
(define-key global-map (kbd "C-w 2") 'ecb-goto-window-methods)
(define-key global-map (kbd "C-w e 1") 'ecb-goto-window-edit1)
(define-key global-map (kbd "C-w e 2") 'ecb-goto-window-edit2)
(define-key global-map (kbd "C-w e 3") 'ecb-goto-window-edit3)
(define-key global-map (kbd "C-w e 4") 'ecb-goto-window-edit4)
(define-key global-map (kbd "C-w e l") 'ecb-goto-window-edit-last)
(define-key global-map (kbd "C-w e s") 'ecb-goto-window-edit-by-smart-selection)

;;cscope

(define-key global-map (kbd "C-w n f") 'cscope-next-file)
(define-key global-map (kbd "C-w p f") 'cscope-prev-file)
(define-key global-map (kbd "C-w n s") 'cscope-next-symbol)
(define-key global-map (kbd "C-w p s") 'cscope-prev-symbol)
(define-key global-map [f1] 'cscope-select-entry-one-window)
(define-key global-map [f2] 'cscope-find-this-symbol)
(define-key global-map [f3] 'cscope-find-functions-calling-this-function)
;(define-key global-map [f2] 'cscope-select-entry-other-window)
;(define-key global-map [f3] 'cscope-find-this-symbol)
(define-key global-map [f4] 'cscope-set-initial-directory)
(define-key global-map "\C-]" 'cscope-find-global-definition)
(define-key global-map "\C-t" 'cscope-pop-mark)

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
(require 'ecb)
(require 'linum-relative)

;; source contol settings
(require 'magit)
(global-set-key "\C-xg" 'magit-status)

(require 'auto-complete)
(require 'autopair)
(require 'yasnippet)
(require 'flycheck)
(global-flycheck-mode t)

(global-set-key [f7] 'find-file-in-repository)

; auto-complete mode extra settings
(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 20)

;; ;; Python mode settings
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq py-electric-colon-active t)
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)

(require 'pydoc)
(defun lookup-pydoc ()
  (interactive)
  (let ((curpoint (point)) (prepoint) (postpoint) (cmd))
    (save-excursion
      (beginning-of-line)
      (setq prepoint (buffer-substring (point) curpoint)))
    (save-excursion
      (end-of-line)
      (setq postpoint (buffer-substring (point) curpoint)))
    (if (string-match "[_a-z][_\\.0-9a-z]*$" prepoint)
        (setq cmd (substring prepoint (match-beginning 0) (match-end 0))))
    (if (string-match "^[_0-9a-z]*" postpoint)
        (setq cmd (concat cmd (substring postpoint (match-beginning 0) (match-end 0)))))
    (if (string= cmd "") nil
      (let ((max-mini-window-height 0))
        (shell-command (concat "pydoc " cmd))))))

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c h") 'lookup-pydoc)))

(add-hook 'python-mode-hook 'anaconda-mode)

(require 'websocket)
;; #ipython notebook
;; M+x ein:notebooklist-open
(require 'ein)  
(setq ein:use-auto-complete t)
;;(setq ein:use-smartrep t)

;; ;; Jedi settings
(require 'epc)
(require 'virtualenv)
(require 'jedi)

;; It's also required to run "pip install --user jedi" and "pip
;; install --user epc" to get the Python side of the library work
;; correctly.
;; With the same interpreter you're using.

;; if you need to change your python intepreter, if you want to change it
;;(setq jedi:server-command
;;      '("python" "/home/andrea/.emacs.d/elpa/jedi-0.1.2/jediepcserver.py"))
(setq jedi:setup-keys t)
(setq jedi:server-command '("/Users/jman/.emacs.d/elpa/jedi-core-20151214.705/jediepcserver.py"))
(setq jedi:complete-on-dot t)
(setq jedi:server-args
      '("--sys-path" "/Users/jman/anaconda/lib/python3.5/site-packages"))

(add-hook 'python-mode-hook
	  (lambda ()
	    (jedi:setup)
	    (jedi:ac-setup)
            (local-set-key "\C-cd" 'jedi:show-doc)))

(add-hook 'python-mode-hook 'auto-complete-mode)

(ido-mode t)

;; -------------------- extra nice things --------------------
;; use shift to move around windows
(windmove-default-keybindings 'shift)
(show-paren-mode t)
 ; Turn beep off
(setq visible-bell nil)

;;
;(setq magit-last-seen-setup-instructions "1.4.0")




