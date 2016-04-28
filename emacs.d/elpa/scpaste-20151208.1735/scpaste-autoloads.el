;;; scpaste-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "scpaste" "scpaste.el" (22306 37516 645698
;;;;;;  800000))
;;; Generated autoloads from scpaste.el

(autoload 'scpaste "scpaste" "\
Paste the current buffer via `scp' to `scpaste-http-destination'.
If ORIGINAL-NAME is an empty string, then the buffer name is used
for the file name.

\(fn ORIGINAL-NAME)" t nil)

(autoload 'scpaste-region "scpaste" "\
Paste the current region via `scpaste'.
NAME is used for the file name.

\(fn NAME)" t nil)

(autoload 'scpaste-index "scpaste" "\
Generate an index of all existing pastes on server on the splash page.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; scpaste-autoloads.el ends here
