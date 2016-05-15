(use-package base16-theme
  :init
  (progn
    (set-mouse-color "white")

    (load-theme 'base16-default-dark t)

    ;; turn off clutter
    (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
    (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
    (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

    (set-face-attribute 'highlight nil :underline nil :foreground nil)
    (set-face-attribute 'mode-line nil :box nil :foreground "#a9a9a9")
    (set-face-attribute 'region nil :box nil :foreground nil :background "#483d8b")))
