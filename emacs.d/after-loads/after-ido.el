(require 'pd/ido)

(ido-mode t)
(ido-everywhere t)

(setq ido-enable-flex-matching t
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window
      ido-use-virtual-buffers t)

(add-hook 'ido-setup-hook 'pd/add-ido-keybindings)
(setq ido-rewrite-file-prompt-functions '(pd/ido-file-prompt-abbreviate-file-name))

(require 'ido-other-window)
