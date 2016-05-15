;; set up elpa
(setq relative-config-dir "~/.emacs.d/")
(setq setup-files-dir "config/")

(load-file (concat relative-config-dir "elpa.el"))

;; use use-package :)
(require 'use-package)
;; massage list of file-names
(setq dot-files 
      (mapcar (lambda (item) (concat relative-config-dir setup-files-dir item))
           (list "evil.el"        ;vimim (vi much improved)
                 "path.el"        ;path
                 "font.el"        ;font
                 "aliases.el"     ;cmd aliases
                 "utilities.el"   ;functions
                 "general.el"     ;general
                 "mu.el"          ;mail
                 "projectile.el"  ;projects
                 "keychain.el"    ;keychain
                 "magit.el"       ;magit
                 "nxml.el"
                 "auto-complete.el"
                 "win.el"
                 "yaml.el"
                 "compile.el"
                 "org.el"
                 "ruby.el"
                 "html.el"
                 "haskell.el"
                 "js.el"
                 "yasnippet.el"
                 "erc.el"
                 "scheme.el"
                 "clojure.el"
                 "md.el"
                 "ido.el"
                 "ace-jump.el"
                 "dired.el"
                 "workgroups.el"
                 "shell.el"
                 "theme.el")))

(dolist (file dot-files) (load-file file))
