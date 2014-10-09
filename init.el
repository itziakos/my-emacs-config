;; configuration

(require 'cl)				; common lisp goodies, loop

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

(require 'el-get)

;; Set up packages
(setq
 my:el-get-packages
 '(el-get
   flycheck
   zencoding-mode
   jedi
   pydoc-info
   magit
   flyspell
   ))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

;; jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; no start up screen
(setq inhibit-startup-screen t)

;; Setup ido mode
(require 'ido)
(ido-mode t)

;; add menu item for recent files
(require 'recentf)
(recentf-mode 1)

;; flyspell setup
(add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
(setq ispell-program-name "aspell")
(setq ispell-dictionary "british")
(require 'flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(setq ispell-list-command "--list")

;; flycheck setup
(add-hook 'after-init-hook 'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save))
(setq flycheck-highlight-mode 'lines)

;; setup tabs
(setq c-basic-indent 2)
(setq tab-width 4)
(setq indent-tabs-mode nil)

;; color shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

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

;; choose your own fonts, in a system dependant way
(if (string-match "apple-darwin" system-configuration)
    (set-face-font 'default "Monaco-13")
  (set-face-font 'default "Monospace-7"))

(line-number-mode 1)			; have line numbers and
(column-number-mode 1)			; column numbers in the mode line

(global-hl-line-mode)			; highlight current line
;;(global-linum-mode 1)			; add line numbers on the left
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(current-language-environment "Greek")
 '(custom-enabled-themes (quote (misterioso)))
 '(ede-project-directories (quote ("c:/Users/itziakos/AppData/Roaming/.emacs.d"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
