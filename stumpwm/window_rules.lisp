;;-------~---~----------~----------~----
;; Window Rules
;;-------~---~----------~----------~----
;; frame raise lock (lock AND raise == jumpto)

(define-frame-preference "web"
                         (0 t t :class "Conkeror"))

(define-frame-preference "email"
                         (0 t t :class "mutt"))

(define-frame-preference "chat"
                         (0 t t :class "irssi"))

(define-frame-preference "chat"
                         (0 t t :class "pidgin"))

;;(setf *window-type-override-list*
;; '((:dialog :instance "bash")))
