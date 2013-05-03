(require 'fuzzy)
(require 'auto-complete-config)

(ac-config-default)

(setq ac-fuzzy-enable t
      ac-auto-start nil
      ac-auto-show-menu nil)

(keydef (ac-complete "C-n") ac-next)
(keydef (ac-complete "C-p") ac-previous)
(keydef (ac-complete "C-l") ac-expand-common)
