(dolist (regexp '("\\.rake$" "Rakefile$" "Guardfile$"
                  "Gemfile$" "\\.gemspec$" "\\.?irbrc$" "\\.rabl$" "\\.ru$"))
  (add-to-list 'auto-mode-alist (cons regexp 'ruby-mode)))

(add-to-list 'auto-mode-alist (cons "\\.md$" 'markdown-mode))

(provide 'pd/auto-mode)
