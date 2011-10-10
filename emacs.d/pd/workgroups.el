(require 'workgroups)

(workgroups-mode 1)

(setq wg-morph-on nil
      wg-file "~/.emacs.d/workgroups-config")

(wg-load wg-file)

(provide 'pd/workgroups)
