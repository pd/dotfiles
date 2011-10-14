(defun pd/add-ruby-keybindings ()
  (pd/enable-newline-and-indent ruby-mode-map))

(add-hook 'ruby-mode-hook 'pd/run-coding-hook)
(add-hook 'ruby-mode-hook 'pd/add-ruby-keybindings)

(setq inf-ruby-first-prompt-pattern "^.*[0-9:]+ *[>*\"'] *"
      inf-ruby-prompt-pattern "\\(^\\(irb(.*)[0-9:]+[>*\"'] *\\)+\\)\\|\\(^.*:[0-9]+.*> *\\)")

(dolist (regexp '("\\.rake$" "Rakefile$" "Guardfile$"
                  "Gemfile$" "\\.gemspec$" "\\.?irbrc$"))
  (add-to-list 'auto-mode-alist (cons regexp 'ruby-mode)))

(provide 'pd/ruby)
