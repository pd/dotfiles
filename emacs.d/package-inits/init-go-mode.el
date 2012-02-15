(defun pd/add-go-keybindings ()
  (pd/enable-newline-and-indent go-mode-map))

(add-hook 'go-mode-hook 'pd/run-coding-hook)
(add-hook 'go-mode-hook 'pd/set-tab-width-2)
(add-hook 'go-mode-hook 'pd/add-go-keybindings)
