(deftheme despot "pd's modifications")

(let* ((background          "#161718")
       (background-lighter  "#1b1d1e")
       (background-lightest "#202324")

       (diff-added   "#4c4")
       (diff-removed "#e44")
       (diff-context "grey70")
       (magit-hilite         background-lighter)
       (magit-hilite-lighter background-lightest))

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
   `(font-lock-fic-face ((t (:foreground "#e62020" :background ,background))))

   `(diff-added   ((t (:foreground ,diff-added   :background ,background :weight bold))))
   `(diff-removed ((t (:foreground ,diff-removed :background ,background :weight bold))))
   `(diff-context ((t (:foreground ,diff-context :background ,background))))

   `(magit-diff-added   ((t (:inherit diff-added-face))))
   `(magit-diff-removed ((t (:inherit diff-removed-face))))
   `(magit-diff-context ((t (:inherit diff-context-face))))

   `(magit-diff-added-highlight   ((t (:inherit magit-diff-added-face   :background ,magit-hilite :weight bold))))
   `(magit-diff-removed-highlight ((t (:inherit magit-diff-removed-face :background ,magit-hilite :weight bold))))
   `(magit-diff-context-highlight ((t (:inherit magit-diff-context-face :background ,magit-hilite))))

   `(magit-diff-file-heading           ((t (:inherit font-lock-keyword-face))))
   `(magit-diff-file-heading-highlight ((t (:inherit magit-diff-file-heading-face :background ,magit-hilite-lighter))))

   `(magit-diff-hunk-heading           ((t (:inherit font-lock-variable-name-face))))
   `(magit-diff-hunk-heading-highlight ((t (:inherit magit-diff-hunk-heading-face :background ,magit-hilite-lighter))))

   `(enh-ruby-op-face ((t (:foreground "#efa566"))))

   `(helm-bookmark-directory ((t (:inherit font-lock-function-name-face))))
   `(helm-buffer-directory ((t (:inherit font-lock-function-name-face))))
   `(helm-ff-directory ((t (:inherit font-lock-function-name-face))))
   `(helm-source-header ((t (:foreground "#66d9ef" :weight bold))))
   `(helm-selection ((t (:background "grey22"))))

   `(powerline-active1 ((t (:foreground "#f92672" :background "grey22"))))
   `(powerline-active2 ((t (:foreground "#66d9ef" :background "grey22"))))

   `(powerline-inactive1 ((t (:background "#000"))))
   `(powerline-inactive2 ((t (:background "#000"))))

   ))

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; eval: (when (fboundp 'hl-line-mode) (hl-line-mode -1))
;; no-byte-compile: t
;; End:
