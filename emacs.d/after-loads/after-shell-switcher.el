(shell-switcher-mode)
(setq shell-switcher-new-shell-function 'shell-switcher-make-shell)
(add-hook 'shell-mode-hook 'shell-switcher-manually-register-shell)

; meh, prolly some way to do it with advice, but I'm lazy.
; I don't need you to prompt me if there are no shells available;
; just make one.
(defun sswitcher--no-more-shell-buffers (&optional other-window)
  "Propose to create a new shell as there is no more to switch to.
If user answers positively, a new shell buffer is created. If
OTHER-WINDOW is nil (the default), the shell buffer is displayed
in the current window. If OTHER-WINDOW is t, change another
window.
"
  (let ((default-directory (or sswitcher--starting-default-directory default-directory)))
    (sswitcher--new-shell other-window)
    (setq sswitcher--starting-default-directory nil)))
