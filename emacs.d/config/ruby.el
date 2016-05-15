(use-package enh-ruby-mode
  :init
  (progn 
    (use-package inf-ruby)

    (use-package haml-mode
      :init 
      (progn 
        (add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))
        (setq haml-backspace-backdents-nesting nil)
        (setq haml-indent-offset 2)))
    
    (use-package rhtml-mode 
      :init 
      (progn 
        (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
        (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . rhtml-mode))))

    ;; file type associations
    (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
    (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

    ;; configure ruby-mode
    (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)

    ;; hooks  
    (add-hook 'enh-ruby-mode-hook 'highlight-todo)

    (setq enh-ruby-program "~/.rbenv/shims/ruby")
    (setq ruby-deep-indent-paren nil)
    (setq ruby-insert-encoding-magic-comment nil)
    (setq end-ruby-deep-indent-paren nil)
    (setq end-ruby-deep-arglist t)
    (setq enh-ruby-hanging-brace-deep-indent-level 2)
    (setq enh-ruby-hanging-brace-indent-level 2)
    (setq enh-ruby-hanging-indent-level 2)
    (setq enh-ruby-hanging-paren-deep-indent-level 2)
    (setq enh-ruby-hanging-paren-indent-level 2)
    (setq enh-ruby-indent-level 2)))
