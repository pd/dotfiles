(defun pd/js2-mode-name ()
  (setq mode-name "js2"))

(after 'eldoc
  (diminish 'eldoc-mode))

(after 'elisp-slime-nav
  (diminish 'elisp-slime-nav-mode))

(after 'js2-mode
  (add-hook 'js2-mode-hook 'pd/js2-mode-name))

(after 'quickref
  (diminish 'quickref-mode))

(after 'paredit
  (diminish 'paredit-mode))

(after 'subword
  (diminish 'subword-mode))

(provide 'pd/diminish)
