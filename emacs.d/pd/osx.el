;; Modifiers!
(setq ns-command-modifier   'meta
      ns-alternate-modifier 'super
      ns-function-modifier  'hyper)

;; 24.3 + OSX = pitiful fullscreen experience.
;; I will never forget you, ns-toggle-fullscreen ...
(keydef "s-S-<return>" toggle-frame-fullscreen)

;; Stop asking me to print.
(keydef "s-p")

;; gank the $PATH from a login shell, in case I launched from the dock
(exec-path-from-shell-initialize)

;; Dunno a better way to do this. Sets the font to (roughly) Monaco 10pt.
;; Default was 12pt.
(set-face-attribute 'default nil :height 100)

;; I launch emacs client using an applescript, which sets cwd to /
;; Rather than learn applescript, I fix it here.
(when (string= default-directory "/")
  (cd (getenv "HOME")))

;; Couple commands that only work on OSX
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
