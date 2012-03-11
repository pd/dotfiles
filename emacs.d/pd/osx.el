(setq ns-command-modifier   'meta
      ns-alternate-modifier 'super)

; gank the $PATH from a login shell, in case I launched from the dock
; http://stackoverflow.com/questions/2266905/emacs-is-ignoring-my-path-when-it-runs-a-compile-command
(let ((path (pd/login-shell-path)))
  (setenv "PATH" (mapconcat 'identity path path-separator))
  (setq exec-path path))

; I launch emacs client using an applescript, which sets cwd to /
; Rather than learn applescript, I fix it here.
(when (string= default-directory "/")
  (cd (getenv "HOME")))

(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
           (shell-quote-argument (buffer-file-name)))))

(defun terminal.app ()
  (interactive)
  (shell-command
   (format "open -a /Applications/Utilities/Terminal.app %s"
           (shell-quote-argument default-directory))))

(provide 'pd/osx)
