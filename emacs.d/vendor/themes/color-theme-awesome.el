;; Copyright 2009, Evan Klitzke

(eval-when-compile
  (require 'color-theme))

(defun color-theme-awesome ()
  (interactive)
  (color-theme-install
   '(color-theme-awesome
     ((foreground-color . "#eeeeec")
      (background-color . "grey12")
      (background-mode . dark)
      (cursor-color . "Wheat"))

     (default ((t (:bold t))))

     (border ((t (:background "#888a85"))))
     (region ((t (:background "#555753"))))
     (mode-line ((t (:foreground "#eeeeec" :background "#555753"))))
     (minibuffer-prompt ((t (:bold t :foreground "SkyBlue" :weight bold))))

     (font-lock-builtin-face ((t (:foreground "PaleGreen"))))
     ;(font-lock-builtin-face ((t (:foreground "#88e07c"))))
     (font-lock-comment-face ((t (:foreground "#93ddff" :italic t))))
     (font-lock-constant-face ((t (:foreground "PaleGreen"))))
     (font-lock-doc-face ((t (:foreground "SkyBlue" :italic t))))
     ;;(font-lock-function-name-face ((t (:foreground "Khaki" :bold t))))
     (font-lock-function-name-face ((t (:foreground "#ffe88c"))))
     (font-lock-keyword-face ((t (:foreground "#ff9e54" :bold t))))
     (font-lock-string-face ((t (:foreground "#bdbdff"))))
     (font-lock-type-face ((t (:foreground "PaleGreen" :bold t))))
     ;(font-lock-variable-name-face ((t (:foreground "#fce94f"))))
     (font-lock-variable-name-face ((t (:foreground "#eeeeec"))))
     (font-lock-warning-face ((t (:bold t :foreground "#ff2b2b"))))
)))

(provide 'color-theme-awesome)
