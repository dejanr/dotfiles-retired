;; ask before closing
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

;; highlight keywords
(defun highlight-todo ()
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|NOTE\\|TODO\\|IDEA\\|BUG\\):"
                             1
                             font-lock-warning-face t))))

(defun switch-server-hook ()
  (when (current-local-map)
    (use-local-map (copy-keymap (current-local-map))))
  (when server-buffer-clients
    (local-set-key (kbd "C-x C-c") 'server-edit))
  (when server-buffer-clients
    (local-set-key (kbd "C-x k") 'server-edit)))

(defun cleanup-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(defun open-all-files-in-dir ()
  (interactive)
  (dolist (file (directory-files default-directory))
    (unless (or (equal file ".") 
                (equal file ".."))
      (find-file file))))

(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)))
