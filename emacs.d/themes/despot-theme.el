(deftheme despot "pd's modifications")

(let ((background          "#161718")
      (background-lighter  "#1b1d1e")
      (background-lightest "#202324"))

  (custom-theme-set-faces
   'despot

   `(default ((t (:background ,background))))
   `(cursor ((t (:background "#FD971F"))))
   `(font-lock-comment-face ((t (:foreground "#656F72"))))

   `(magit-item-highlight ((t (:background ,background-lighter :inverse-video nil))))
   `(magit-section-title ((t (:inherit font-lock-type-face))))
   `(magit-branch ((t (:inherit font-lock-type-face))))

   `(diff-file-header ((t (:inherit font-lock-delimiter-face :background ,background))))
   `(diff-hunk-header ((t (:inherit font-lock-type-face :background ,background))))

   `(diff-added ((t (:foreground "#a6e22e" :background ,background :weight normal))))
   `(diff-removed ((t (:foreground "#e62020" :background ,background))))
   `(diff-refine-change ((t (:background ,background))))

   `(font-lock-fic-face ((t (:foreground "#e62020" :background ,background))))

   ))
