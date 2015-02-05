(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

(add-to-list 'load-path (concat user-emacs-directory "config"))

(require 'cl)

(require 'init-packages)
(require 'init-core)
(require 'init-theme)
(require 'init-yasnippet)
(require 'init-git)
(require 'init-lisp)
(require 'init-markdown)
(require 'init-bindings)

(setq ns-use-native-fullscreen nil)
