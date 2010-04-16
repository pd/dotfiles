(require 'color-theme)

(setq color-theme-history-max-length t
      color-theme-is-global nil
      color-theme-is-cumulative nil)

(defun pd/set-color-theme (&optional frame)
  "Set the color theme only when in a window system
This is run from a hook that isn't called for the first frame,
but since I use emacsd 99% of the time that's not much of an issue"
  (let ((color-theme-is-global nil))
    (select-frame frame)
    (when window-system
      (color-theme-despot))))

(defun color-theme-despot ()
  "color-theme for pd. mostly inkpot."
  (interactive)
  (color-theme-install
   '(color-theme-despot

     ((foreground-color . "#cfbfad")
      (background-color . "#000000")
      (border-color . "#3e3e5e")
      (cursor-color . "#404040")
      (background-mode . dark))

     (region ((t (:background "#303030"))))
     (highlight ((t (:background "#005bd4"))))
     (hl-line ((t (:background "#1f1f1b"))))
     (fringe ((t (:background "#090909"))))

     (font-lock-builtin-face ((t (:foreground "#c080d0"))))
     (font-lock-comment-face ((t (:foreground "#708090" :italic t))))
     (font-lock-constant-face ((t (:foreground "#506dbd"))))
     (font-lock-doc-face ((t (:foreground "#cd8b00"))))
     (font-lock-function-name-face ((t (:foreground "#87cefa"))))
     (font-lock-keyword-face ((t (:bold t :foreground "#c080d0"))))
     (font-lock-preprocessor-face ((t (:foreground "309090"))))
     (font-lock-reference-face ((t (:bold t :foreground "#808bed"))))
     (font-lock-string-face ((t (:foreground "#ffcd8b"))))
     (font-lock-type-face ((t (:foreground "#ff8bff"))))
     (font-lock-variable-name-face ((t nil)))
     (font-lock-warning-face ((t (:foreground "#ffffff" :background "#ff0000"))))

     (comint-highlight-prompt ((t (:foreground "steel blue"))))
     (comint-highlight-input ((t (:italic t :bold t))))

     (minibuffer-prompt ((t (:bold t :foreground "steel blue"))))

     (isearch ((t (:bold t :foreground "#090909" :background "#3e3e5e"))))

     (modeline ((t (:bold t :foreground "#b9b9b9" :background "#3e3e5e"))))
     (modeline-inactive ((t (:bold t :foreground "#708090" :background "#303030"))))

     (show-paren-match-face ((t (:foreground "steel blue"))))

     (magit-diff-add ((t (:foreground "green3"))))
     (magit-diff-del ((t (:foreground "red3"))))

     (erc-input-face ((t (:foreground "light steel blue"))))
     (erc-my-nick-face ((t (:foreground "steel blue"))))
     (erc-timestamp-face ((t (:foreground "grey50"))))

     )))

(add-hook 'after-make-frame-functions 'pd/set-color-theme)

(provide 'pd/theme)
