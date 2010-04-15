; color themes are often ugly in terminals.
; i rarely if ever run emacs outside of an x11 frame,
; so only use color themes if using a windowing system.

(when window-system
  (require 'color-theme)
  (setq color-theme-history-max-length t) ; unlimited
  (pd/load-directory "~/.emacs.d/vendor/themes"))

(defun pd/theme-tweak-erc ()
  (eval-after-load 'erc-stamp
    '(progn
       (set-face-foreground 'erc-input-face "light steel blue")
       (set-face-foreground 'erc-my-nick-face "steel blue")
       (set-face-foreground 'erc-timestamp-face "grey50"))))

(defun pd/theme-tweak-magit ()
  (eval-after-load 'magit
    '(progn
       (set-face-background 'magit-item-highlight "gray12")
       (set-face-foreground 'magit-diff-add "green3")
       (set-face-foreground 'magit-diff-del "red3"))))

(when window-system
  (color-theme-inkpot)
  (pd/theme-tweak-erc)
  (pd/theme-tweak-magit))

(provide 'pd/theme)
