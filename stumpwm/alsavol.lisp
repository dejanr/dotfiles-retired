;;; alsavol.lisp --- simple StumpWM module to interact with ALSA

;;; Copyright (C) 2014 Trevor Murphy

;;; This program is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation, either version 3 of the License, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.

;;; You should have received a copy of the GNU General Public License along
;;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; Commentary:

;;; Usage
;;; -----

;;; The following commands are defined:

;;; + alsavol-vol+
;;; + alsavol-vol-
;;; + alsavol-toggle-mute
;;; + alsavol-interactive
;;; + alsavol-exit-interactive
;;; + alsavol-application-list

;;; The first three commands are supposed to be used via multimedia keys.  You
;;; probably want to assign appropriate keys in your `.stumpwmrc', e.g.

;;;   (define-key *top-map*
;;;               (kbd "XF86AudioRaiseVolume")
;;;               "alsavol-vol+")

;;; `alsavol-interactive' can be used if you don't have or don't want to use
;;; the media keys.  Once called, you can use `j', `k' and `m' keys to control
;;; the volume and exit the interactive mode using `ESC', `RET' or `C-g'.

;;; With `alsavol-control-list' you can list the mixer controls that can have
;;; their volume changed by amixer.  Navigate the menu using `j' and `k' keys
;;; and select the desired control using `RET'.

;;;; Code:

(defpackage #:alsavol
  (:use #:cl))

(in-package #:alsavol)

(defparameter *alsavol-keymap*
  (let ((m (stumpwm:make-sparse-keymap)))
    (labels ((dk (k c)
               (stumpwm:define-key m k c)))
      (dk (stumpwm:kbd "j") "alsavol-vol-")
      (dk (stumpwm:kbd "k") "alsavol-vol+")
      (dk (stumpwm:kbd "m") "alsavol-toggle-mute")
      (dk (stumpwm:kbd "RET") "alsavol-exit-interactive")
      (dk (stumpwm:kbd "C-g") "alsavol-exit-interactive")
      (dk (stumpwm:kbd "ESC") "alsavol-exit-interactive")
      m)))

(defparameter *alsavol-application-list-keymap*
  (let ((m (stumpwm::copy-kmap stumpwm::*menu-map*)))
    (labels ((dk (k c)
               (stumpwm:define-key m k c)))
      (dk (stumpwm:kbd "j") 'stumpwm::menu-down)
      (dk (stumpwm:kbd "k") 'stumpwm::menu-up)
      m)))

(defvar *alsavol-control* "Master"
  "The control used when changing the volume or muting a sink")

(defun set-interactive (&optional (control "Master"))
  (setf *alsavol-control* control)
  (stumpwm::push-top-map *alsavol-keymap*))

(defun unset-interactive ()
  (setf *alsavol-control* "Master")
  (stumpwm::pop-top-map))

(defun interactivep ()
  (equal stumpwm:*top-map* *alsavol-keymap*))

(defun control-list-table ()
  (let (table)
    (ppcre:do-matches-as-strings
        (name "\\w+(?= Playback Volume)"
              (stumpwm:run-shell-command "amixer contents" t)
              (nreverse table))
      (push (cons
             (format nil "~10a ~:[OPEN ~;MUTED~] ~a"
                     name (mutep name) (make-volume-bar (volume name)))
             name) table))))

(defun volume (&optional (control *alsavol-control*))
  (ppcre:register-groups-bind (volume)
      ("\\[(.*?)%\\]"
       (stumpwm:run-shell-command
        (format nil "amixer get ~a" control) t))
    (when volume
      (parse-integer volume))))

(defun mutep (&optional (control *alsavol-control*))
  (ppcre:register-groups-bind (mutep)
      ("\\[(on|off)\\]"
       (stumpwm:run-shell-command
        (format nil "amixer get ~a" control) t))
    (when mutep
      (string= "off" mutep))))

(defun set-volume (percentage &optional (change 0))
  (let ((sign (cond
                ((> change 0) "+")
                ((< change 0) "-")
                (t ""))))
    (stumpwm:run-shell-command
     (format nil "amixer set ~a ~a%~a"
             *alsavol-control* percentage sign))))

(defun toggle-mute-1 (&optional (control *alsavol-control*))
  (stumpwm:run-shell-command
   (format nil "amixer set ~a toggle" control)))

(defun make-volume-bar (percent)
  (format nil "^B~3d%^b [^[^7*~a^]]"
          percent (stumpwm::bar percent 50 #\# #\:)))

(defun show-volume-bar (&optional (control *alsavol-control*))
  (funcall (if (interactivep)
               #'stumpwm::message-no-timeout
               #'stumpwm:message)
           (format nil "~a ~:[OPEN~;MUTED~]~%~a"
                   control (mutep control)
                   (make-volume-bar (volume control)))))

(defun volume-up (percentage)
  (set-volume percentage +1)
  (show-volume-bar))

(defun volume-down (percentage)
  (set-volume percentage -1)
  (show-volume-bar))

(defun toggle-mute ()
  (toggle-mute-1)
  (show-volume-bar))

;;;; Commands

(stumpwm:defcommand alsavol-vol+ () ()
  "Increase the volume by ~5%."
  (volume-up 5))

(stumpwm:defcommand alsavol-vol- () ()
  "Decrease the volume by ~5%."
  (volume-down 5))

(stumpwm:defcommand alsavol-toggle-mute () ()
  "Toggle mute."
  (toggle-mute))

(stumpwm:defcommand alsavol-exit-interactive () ()
  "Exit the interactive mode for changing the volume"
  (unset-interactive))

(stumpwm:defcommand alsavol-interactive () ()
  "Change the volume interactively using `j', `k' and `m' keys"
  (set-interactive)
  (show-volume-bar))

(stumpwm:defcommand alsavol-control-list () ()
  "Choose from a list of simple mixer controls."
  (let ((controls (control-list-table)))
    (if (null controls)
        (stumpwm:message "No application is running")
        (let* ((stumpwm::*menu-map* *alsavol-application-list-keymap*)
               (sink (stumpwm::select-from-menu (stumpwm:current-screen)
                                                controls)))
          (when sink
            (set-interactive (cdr sink))
            (show-volume-bar))))))
