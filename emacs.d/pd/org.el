(require 'org)

(setq org-startup-folded 'showeverything
      org-agenda-files '("~/org"))

(global-set-key (kbd "C-c a") 'org-agenda)

(provide 'pd/org)
