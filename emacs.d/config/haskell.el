(use-package haskell-mode
  :init
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
    ))
