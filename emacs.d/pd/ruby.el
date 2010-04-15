;; TODO: should install these from elpa
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby" t)
(autoload 'run-ruby "inf-ruby" "Inferior mode for ruby" t)

(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.rake\\'" . ruby-mode)
                ("Rakefile\\'" . ruby-mode))))

(defun pd/add-ruby-keybindings ()
  (pd/enable-newline-and-indent ruby-mode-map))

(add-hook 'ruby-mode-hook 'pd/run-coding-hook)
(add-hook 'ruby-mode-hook 'pd/add-ruby-keybindings)

(provide 'pd/ruby)
