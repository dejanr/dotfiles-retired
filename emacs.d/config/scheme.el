(use-package chicken-scheme
  :init
  (progn
    (setq scheme-program-name "csi -:c")
    (setq scheme-tags-location "~/.emacs.d/scheme-tags")
    (setq cicken-scheme-tags-file "~/.emacs.d/chicken-scheme-tags")

    (defun scheme-module-indent (state indent-point normal-indent) 2)
    (put 'module 'scheme-indent-function 'scheme-module-indent)
    (put 'handle-exceptions 'scheme-indent-function 1)
    (put 'make-request 'scheme-indent-function 1)
    (put 'and-let* 'scheme-indent-function 1)
    (put 'parameterize 'scheme-indent-function 1)
    (put 'when 'scheme-indent-function 1)
    (put 'unless 'scheme-indent-function 1)
    (put 'match 'scheme-indent-function 1)
    (put 'let-values 'scheme-indent-function 1)))
