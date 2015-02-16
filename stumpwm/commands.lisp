;; To-and-Fro Group switching (like i3)
(defcommand go-group (n) ((:number "Go to group: "))
            "Go to selected group, or back to last used one"
            (if (= (slot-value (current-group) 'number) n)
              (gother)
              (run-commands (format nil "gselect ~A" n))))

(defcommand send-selection nil nil
            (window-send-string (get-x-selection)))

(defcommand gimp () ()
            "Run-or-Raise Gimp"
            (run-or-raise "gimp" '(:class "Gimp")))

(defcommand mutt () ()
            "Run-or-Raise Mutt"
            (run-or-raise (format
                            nil
                            "exec ~A -name mutt -e mutt -F ~A/muttrc"
                            *terminal* (getenv "HOME"))
                          '(:instance "mutt")))

(defcommand rotate-windows () ()
  (let* ((frames (group-frames (current-group)))
           (win (frame-window (car (last frames)))))
          (shift-windows-forward frames win)))

;; toggle between vertical split and horizontal split
(defcommand toggle-split () ()
  (let* ((group (current-group))
         (cur-frame (tile-group-current-frame group))
         (frames (group-frames group)))
    (if (eq (length frames) 2)
        (progn (if (or (neighbour :left cur-frame frames)
                       (neighbour :right cur-frame frames))
                   (progn
                     (only)
                     (vsplit))
                 (progn
                   (only)
                   (hsplit))))
      (message "Works only with 2 frames"))))
