(require 'cl)
(require 'package)

(setq package-user-dir (concat relative-config-dir "elpa"))

(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))

(package-initialize)

;; set the dir where all elpa packages go

(setq config:packages '(use-package))

(defun config:install-packages ()
    (let ((pkgs (remove-if #'package-installed-p config:packages)))
        (when pkgs
            (message "%s" "Emacs refresh packages database...")
            (package-refresh-contents)
            (message "%s" " done.")
            (dolist (p config:packages)
                (package-install p)))))

(config:install-packages)

;; ensure that all used packages are installed
(setq use-package-always-ensure t)
