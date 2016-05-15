(use-package better-defaults)
(use-package scpaste)

(load-theme 'wombat t)

;; default tab-width
(setq-default tab-width 2)

;; always indent with spaces
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 2 60 2))

;; when ever a change to a file occurs, reflect it in the buffer
(global-auto-revert-mode t)

;; always highlight current line
(global-hl-line-mode t)

;; utf8 all the way
(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; don't show the startup message
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

;; set default directory
(setq default-directory "~/")
(setq default-major-mode 'org-mode)     ;default is org-mode
(setq initial-major-mode 'org-mode)     ;scratch buffer as well
(setq initial-scratch-message "")

;; don't want this binding at all (lead to 'emacs release news')
(global-set-key (kbd "C-h C-n") nil)

;; global ace jump thing
(global-set-key (kbd "C-x b") 'switch-to-buffer)

;; dont backup for now
(setq make-backup-files nil)

;; don't just close emacs
(global-set-key (kbd "C-x C-c") 'ask-before-closing)

;; atreus binding
(global-set-key (kbd "C-x '") 'delete-other-windows)
(global-set-key (kbd "C-x ,") 'split-window-below)
(global-set-key (kbd "C-x .") 'split-window-right)
(global-set-key (kbd "C-x l") 'delete-window)
