(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			("marmalade" . "https://marmalade-repo.org/packages/")
			("melpa" . "https://melpa.org/packages/")
			("org" . "http://orgmode.org/elpa/")))

(defvar my-packages '(better-defaults paredit idle-highlight-mode ido-ubiquitous
                                      find-file-in-project magit smex scpaste))

(package-initialize)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'evil)
(evil-mode 1)
