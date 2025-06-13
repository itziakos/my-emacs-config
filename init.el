;; Temporarily increase GC threshold during startup
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 100 1024 1024)))) ; 100 MB for interactive use

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

;; Make sure that use-package is always available
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;; use-package should install packages
(setq use-package-always-ensure t) ;

;; Add packages
(use-package jtags)
(use-package log4j-mode)
(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))
(use-package emmet-mode)
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))
(use-package smex
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands))
  :config
  (setq smex-save-file "~/.emacs.d/.smex-items"))
(use-package cython-mode)
(use-package plantuml-mode)
(use-package monky)
(use-package yaml-mode)
(use-package ido
  :init (ido-mode t) ; Init code runs before package is loaded
  :config
  (setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point 'guess)
  (setq ido-show-dot-for-dired t)
  (setq ido-default-buffer-method 'selected-window))
(use-package flyspell
  :hook ((text-mode . turn-on-flyspell) ; Load flyspell on text-mode
         (python-mode . flyspell-prog-mode)) ; And python-mode
  :config
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "british")
  (setq ispell-list-command "--list"))
(use-package flycheck
  :init (global-flycheck-mode) ; This will run global-flycheck-mode on startup, but flycheck itself is still loaded
  :config
  (setq flycheck-check-syntax-automatically '(save))
  (setq flycheck-highlight-mode 'lines))
(use-package term
  :commands (term ansi-term) ; Defer loading until one of these commands is called
  :config
  (define-key term-raw-map (kbd "C-'") 'term-line-mode)
  (define-key term-mode-map (kbd "C-'") 'term-char-mode))
(require 'dired-x)

;; default key to switch buffer is C-x b, but that's not easy enough
;;
;; when you do that, to kill emacs either close its frame from the window
;; manager or do M-x kill-emacs.  Don't need a nice shortcut for a once a
;; week (or day) action.
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-c") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

;; no start up screen
(setq inhibit-startup-screen t)

;; copy/paste with C-c and C-v and C-x, check out C-RET too
(cua-mode)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; setup tabs
(setq c-basic-indent 2)
(setq tab-width 4)
(setq indent-tabs-mode nil)

;; M-x shell is a nice shell interface to use, let's make it colorful.  If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

;; whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes in the corresponding buffer, just revert its
;; content to reflect what's on-disk.
(global-auto-revert-mode 1)

;; save temporary files in a temp directory
(setq backup-directory-alist
`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
`((".*" ,temporary-file-directory t)))

;;setup layout
(tool-bar-mode -1)			; no tool bar with icons
(scroll-bar-mode -1)			; no scroll bars
(unless (string-match "apple-darwin" system-configuration)
  ;; on mac, there's always a menu bar drown, don't have it empty
  (menu-bar-mode -1))

;; Choose the fonts, in a system dependant way and based on the resolution
(defun fontify-frame (&optional width)
  (interactive "p")
  (let ((os system-configuration)
	(target (if width width (nth 3 (car (frame-monitor-attributes))))))
    (if window-system
	(let ((apple-os (string-match "apple-darwin" os))
	      (windows-os (string-match "w64" os)))
	  (let ((selected-font
		 (cond
		  ((> target 2000)
		   (if apple-os "Monospace-16" (if windows-os "consolas-16" "Monospace-16")))
		  ((> target 1200)
		   (if apple-os "Monospace-14" (if windows-os "consolas-14" "Monospace-14")))
		  ((> target -1)
		   (if apple-os "Monospace-11" (if windows-os "consolas-11" "Monospace-11"))))))
	    (set-frame-parameter nil 'font selected-font))))))

;; Fontify current frame
(fontify-frame nil)
;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

(line-number-mode 1)			; have line numbers and
(column-number-mode 1)			; column numbers in the mode line

(global-hl-line-mode)			; highlight current line
(global-display-line-numbers-mode 1)	; add line numbers on the left


(defun my-delete-trailing-whitespace-except-diff-modes ()
  "Delete trailing whitespace, but not in diff-related modes."
  (unless (or (eq major-mode 'diff-mode)
              (eq major-mode 'ediff-mode)
              ;; Add other diff-like modes if you use them, e.g., 'vc-diff-mode
              )
    (delete-trailing-whitespace)))

;; Add the new function to the hook
(add-hook 'before-save-hook 'my-delete-trailing-whitespace-except-diff-modes)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf"
    "#eeeeec"])
 '(blink-cursor-mode nil)
 '(current-language-environment "Greek")
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
