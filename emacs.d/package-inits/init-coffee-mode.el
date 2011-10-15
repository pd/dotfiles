(defun pd/add-coffee-keybindings ()
  (pd/enable-newline-and-indent coffee-mode-map))

(add-hook 'coffee-mode-hook 'pd/set-tab-width-2)
(add-hook 'coffee-mode-hook 'pd/run-coding-hook)
(add-hook 'coffee-mode-hook 'pd/add-coffee-keybindings)
(add-hook 'coffee-mode-hook 'pd/turn-on-show-paren-mode)
