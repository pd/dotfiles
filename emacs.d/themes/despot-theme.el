(deftheme despot "pd's modifications")

(let ((background          "#161718")
      (background-lighter  "#1b1d1e")
      (background-lightest "#202324"))

  (custom-theme-set-faces
   'despot

   `(default ((t (:background ,background))))
   `(cursor ((t (:background "#fd971f"))))

   `(font-lock-builtin-face ((t (:foreground "#f92672"))))
   `(font-lock-comment-face ((t (:foreground "#656f72"))))
   `(font-lock-function-name-face ((t (:foreground "#a25e25"))))
   `(font-lock-keyword-face ((t (:foreground "#3591a2"))))
   `(font-lock-variable-name-face ((t (:foreground "#efa566"))))
   `(font-lock-type-face ((t (:foreground "#66d9ef"))))

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
