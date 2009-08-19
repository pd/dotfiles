; turn everything off.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq visible-bell t)
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'cl)
(require 'ido)
(require 'uniquify)
(require 'ansi-color)

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)

(setq-default indent-tabs-mode nil
	      show-trailing-whitespace t)

(setq column-number-mode t
      require-final-newline t
      uniquify-buffer-name-style 'forward) ; a/b, c/b, not b<2>

(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "<f6>") 'linum-mode)

(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b b") 'bury-buffer)
(global-set-key (kbd "C-x C-b l") 'list-buffers)

(windmove-default-keybindings)

(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
