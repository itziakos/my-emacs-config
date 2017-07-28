
;;;; package ---- configuration

(require 'cl)				; common lisp goodies, loop

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

(require 'el-get)

;; intall log4j from the elpa repo
(el-get-bundle elpa:jtags)
(el-get-bundle elpa:log4j-mode)

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
   smex
   cython-mode
   plantuml-mode
   monky
   yaml-mode
   ))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

;; jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:environment-root "default")  ; or any other name you like
(jedi:install-server)  ; or any other name you like


;; no start up screen
(setq inhibit-startup-screen t)

(setq smex-save-file "~/.emacs.d/.smex-items")
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Setup ido mode
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)
(setq ido-default-buffer-method 'selected-window)
;; have vertical ido completion lists
;;(setq ido-decorations
;;      '("\n-> " "" "\n   " "\n   ..." "[" "]"
;;	" [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; default key to switch buffer is C-x b, but that's not easy enough
;;
;; when you do that, to kill emacs either close its frame from the window
;; manager or do M-x kill-emacs.  Don't need a nice shortcut for a once a
;; week (or day) action.
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-c") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

;; copy/paste with C-c and C-v and C-x, check out C-RET too
(cua-mode)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

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

;; M-x shell is a nice shell interface to use, let's make it colorful.  If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; If you do use M-x term, you will notice there's line mode that acts like
;; emacs buffers, and there's the default char mode that will send your
;; input char-by-char, so that curses application see each of your key
;; strokes.
;;
;; The default way to toggle between them is C-c C-j and C-c C-k, let's
;; better use just one key to do the same.
(require 'term)
(define-key term-raw-map  (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)

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
(global-linum-mode 1)			; add line numbers on the left

;; Remove trailing whitespace on save
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
 '(ede-project-directories
   (quote
    ("c:/Users/itziakos/AppData/Roaming/.emacs.d"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
