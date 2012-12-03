(setq ns-command-modifier   'meta
      ns-alternate-modifier 'super
      ns-function-modifier 'hyper)

; gank the $PATH from a login shell, in case I launched from the dock
(exec-path-from-shell-initialize)

; I launch emacs client using an applescript, which sets cwd to /
; Rather than learn applescript, I fix it here.
(when (string= default-directory "/")
  (cd (getenv "HOME")))

; battery power in my modeline. yay.
(display-battery-mode 1)
(setq battery-mode-line-format " [%b%p %t]")

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

(defun iterm ()
  (interactive)
  (shell-command
   (format "open -a /Applications/iTerm.app %s"
           (shell-quote-argument default-directory))))

(provide 'pd/osx)
