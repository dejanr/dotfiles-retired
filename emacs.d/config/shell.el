(use-package shell
  :init
  (progn
    (global-set-key (kbd "C-x C-t") 'ansi-term)
    (setq explicit-shell-file-name "/run/current-system/sw/bin/bash")
    (setq shell-file-name "/run/current-system/sw/bin/bash")
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))
