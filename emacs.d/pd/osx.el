(setq ns-command-modifier   'meta
      ns-alternate-modifier 'super)

; gank the $PATH from a login shell, in case I launched from the dock
; http://stackoverflow.com/questions/2266905/emacs-is-ignoring-my-path-when-it-runs-a-compile-command
(let ((path (pd/login-shell-path)))
  (setenv "PATH" (mapconcat 'identity path path-separator))
  (setq exec-path path))

(provide 'pd/osx)
