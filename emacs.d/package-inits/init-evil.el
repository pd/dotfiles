(add-to-list 'evil-emacs-state-modes 'el-get-package-menu-mode)

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
