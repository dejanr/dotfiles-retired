(in-package :stumpwm)

(load-module "cpu")
(load-module "mem")

(setf stumpwm:*screen-mode-line-format*
      (list
       "[^B%n^b] " ; group num
       ;; notification
       "^13%u^n"
       "^B%w^b"

       "^>" ; right align
       "%c" ; cpu
       "| %M " ; mem
       '(:eval (string-right-trim '(#\Newline) (run-shell-command "date +'| %A %d %b ^7*^B%H:%M^b^n'" t)))
       ))

(dolist (head
          (list (first (screen-heads (current-screen)))) ; first
         ;; (screen-heads (current-screen)) ; all
         )
(enable-mode-line (current-screen) head t *screen-mode-line-format*))
