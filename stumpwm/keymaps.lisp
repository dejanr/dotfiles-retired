;; Groups
(dotimes (i 9)
  (define-key *top-map* (kbd (format nil "s-~A" (1+ i)))
              (format nil "go-group ~A" (1+ i)))
  (define-key *top-map* (kbd (format nil "s-M-~A" (1+ i)))
              (format nil "gmove ~A" (1+ i))))

;; Frames
(define-key *root-map* (kbd "h") "move-focus left")
(define-key *root-map* (kbd "l") "move-focus right")
(define-key *root-map* (kbd "k") "move-focus up")
(define-key *root-map* (kbd "j") "move-focus down")
(define-key *root-map* (kbd "H") "move-window left")
(define-key *root-map* (kbd "L") "move-window right")
(define-key *root-map* (kbd "K") "move-window up")
(define-key *root-map* (kbd "J") "move-window down")
(define-key *root-map* (kbd "s") "vsplit")
(define-key *root-map* (kbd "v") "hsplit")
(define-key *root-map* (kbd "|") "toggle-split")
(define-key *root-map* (kbd "w") "frame-windowlist")
(define-key *root-map* (kbd "C-w") "windowlist")

;; Top-map
(define-key *top-map* (kbd "s-q")        "delete")
(define-key *top-map* (kbd "s-w")        "remove")
(define-key *top-map* (kbd "s-R")        "loadrc")
(define-key *top-map* (kbd "s-Q")        "restart")
(define-key *top-map* (kbd "s-TAB")      "pull-hidden-next")
(define-key *top-map* (kbd "s-RET")      (format nil "exec ~A" *terminal*))

;; Multimedia Keys
(define-key *top-map* (kbd "XF86AudioPlay")        "exec mpc toggle")
(define-key *top-map* (kbd "XF86AudioNext")        "exec mpc next")
(define-key *top-map* (kbd "XF86AudioPrev")        "exec mpc prev")
(define-key *top-map* (kbd "XF86AudioStop")        "exec mpc stop")

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "alsavol-vol+")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "alsavol-vol-")
(define-key *top-map* (kbd "XF86AudioMute")        nil)

;; Vim-ify menu
(setf *menu-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "k")   'menu-up)
        (define-key m (kbd "j")   'menu-down)
        (define-key m (kbd "SPC") 'menu-finish)
        (define-key m (kbd "RET") 'menu-finish)
        (define-key m (kbd "ESC") 'menu-abort)
        m))

(defvar *group-map* nil
  "Keymap for doing stuffs to groups")
(setf *group-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "n")   "gnew")
        (define-key m (kbd "f")   "gnew-float")
        (define-key m (kbd "N")   "gnewbg")
        (define-key m (kbd "F")   "gnewbg-float")
        (define-key m (kbd "k")   "gkill")
        (define-key m (kbd "m")   "gmove")
        (define-key m (kbd "r")   "grename")
        m))
(define-key *top-map* (kbd "s-g") '*group-map*)

;; Application Launchers
(defvar *launch-map* nil
  "Keymap for launching stuffs")
(setf *launch-map*
      (let ((m (make-sparse-keymap)))
      (define-key m (kbd "b")   "exec conkeror")
      (define-key m (kbd "a")   (format nil "exec ~A -e alsamixer" *terminal*))
      m))
(define-key *top-map* (kbd "s-SPC") '*launch-map*)

(defvar *query-map* nil
  "Keymap for searching the webs")
(setf *query-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "i") "imdb")
        (define-key m (kbd "g") "google")
        (define-key m (kbd "w") "wikipedia")
        (define-key m (kbd "y") "youtube")
        (define-key m (kbd "b") "bbs")
        (define-key m (kbd "t") "bbsa")
        (define-key m (kbd "a") "awiki")
        (define-key m (kbd "c") "pac")
        (define-key m (kbd "l") "last.fm")
        m))
(define-key *top-map* (kbd "s-S") '*query-map*)
