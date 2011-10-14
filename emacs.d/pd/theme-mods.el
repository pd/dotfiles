(defun pd/color-theme-mods ()
  "Additional modifications to my theme, so I needn't fork a project"
  (interactive)
  (color-theme-install
   ; from sanityinc-dark
   (let ((med-dark-choc    "#855c1b")
         (dark-aluminium   "#61635e")
         (dark-chameleon   "#4e9a06")
         (medium-chameleon "#73d216")
         (plum             "#77507b")
         (dark-butter      "#c4a000")
         (light-sky-blue   "#729fcf")
         (slate-blue       "#463b8a")
         (aluminium        "#888a85")
         (light-plum       "#ad7fa8")
         (scarlet          "#cc0000")
         (foreground-white "#d3d7cf")
         (background-black "#000")
         (very-dark-grey   "#0f0f0f")
         (dark-grey        "#1f1f1f")
         (light-grey       "#2e3436")
         (very-light-grey  "#eeeeec"))
       `(pd/color-theme-mods
         nil
         (magit-item-highlight ((t (:background ,very-dark-grey))))
         (magit-section-title ((t (:background ,very-dark-grey))))
         (magit-branch ((t (:foreground ,plum :background ,background-black))))

         (diff-file-header ((t (:foreground ,aluminium :background nil :weight normal))))
         (diff-hunk-header ((t (:foreground ,dark-aluminium :background nil :weight normal))))
         (diff-added ((t (:foreground ,medium-chameleon :background ,background-black :weight normal))))
         (diff-removed ((t (:foreground ,scarlet :background ,background-black :weight normal))))))))

(provide 'pd/theme-mods)
