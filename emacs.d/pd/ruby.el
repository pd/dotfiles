(defun pd/add-ruby-keybindings ()
  (pd/enable-newline-and-indent ruby-mode-map))

(add-hook 'ruby-mode-hook 'pd/run-coding-hook)
(add-hook 'ruby-mode-hook 'pd/add-ruby-keybindings)
(add-hook 'ruby-mode-hook 'ruby-tools-mode)

(setq inf-ruby-first-prompt-pattern "^.*[0-9:]+ *[>*\"'] *"
      inf-ruby-prompt-pattern "\\(^\\(irb(.*)[0-9:]+[>*\"'] *\\)+\\)\\|\\(^.*:[0-9]+.*> *\\)")

; xmpfilter
(defun xmpfilter ()
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region beg end "xmpfilter" nil 'replace)))

(provide 'pd/ruby)
