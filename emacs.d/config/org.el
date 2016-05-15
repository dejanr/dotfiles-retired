(use-package org
  :init
  (progn 
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    (add-hook 'org-mode-hook 'flyspell-mode)))
