(use-package ace-jump-mode
  :init
  (progn
    (global-set-key (kbd "C-c f") 'ace-jump-word-mode)
    (global-set-key (kbd "C-c w") 'ace-jump-word-mode)
    (global-set-key (kbd "C-x C-b") 'ace-jump-buffer)))
