(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window
      ido-use-virtual-buffers t)

(require 'ido-other-window)
