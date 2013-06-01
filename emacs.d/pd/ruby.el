(after 'ruby-mode
  (add-hook 'ruby-mode-hook 'ruby-tools-mode)
  (add-hook 'ruby-mode-hook 'subword-mode)
  (add-hook 'ruby-mode-hook 'yard-mode)
  (add-hook 'ruby-mode-hook 'eldoc-mode)
  (add-hook 'ruby-mode-hook 'robe-mode)

  (global-rinari-mode +1)
  (defun rinari-script-path ()
    "Override to check RAILS_ROOT/bin instead of RAILS_ROOT/script"
    (concat (file-name-as-directory (expand-file-name "bin" (rinari-root))))))

(after 'inf-ruby
  (add-hook 'inf-ruby-mode-hook 'pd/comint-disable-echo))

(after 'robe
  (push 'ac-source-robe ac-sources))

(provide 'pd/ruby)
