(add-to-list 'evil-emacs-state-modes 'el-get-package-menu-mode)
(add-to-list 'evil-emacs-state-modes 'magit-log-edit-mode)
(add-to-list 'evil-emacs-state-modes 'org-mode)

; Enable evil mode by default if I'm probably at work
(defun pd/is-weekday-p ()
  (let ((dow (nth 6 (decode-time))))
    (and (>= dow 1) (<= dow 6))))

(defun pd/is-work-hours-p ()
  (let ((hour (nth 2 (decode-time))))
    (and (>= hour 8) (<= hour 20))))

(defun pd/should-be-evil-p ()
  (and (pd/is-weekday-p) (pd/is-work-hours-p)))

; Remove all 'quitting' from ex-mode, tho. Why the shit does it try to
; close emacs? I never ever ever want that ...
(defun pd/evil-quit-command-p (entry)
  (let ((command (car entry))
        (fn (cdr entry)))
    (or (string-match "^q" command)
        (string-match "^wq" command)
        (string-match "^xa" command))))

(setq evil-ex-commands
      (remove-if 'pd/evil-quit-command-p evil-ex-commands))

(set-default 'evil-shift-width 2)
