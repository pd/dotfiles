; shush.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq visible-bell t)
(defalias 'yes-or-no-p 'y-or-n-p)

; and pile your junk somewhere else.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      tramp-backup-directory-alist '(("." . "~/.emacs.d/backups"))
      save-place-file "~/.emacs.d/places"
      custom-file "~/.emacs.d/custom.el"
      auto-save-default nil)

; religion:
;   1. spaces not tabs
;   2. no excess whitespace
(setq-default indent-tabs-mode nil
              show-trailing-whitespace t)

; utf-8 seems the least wrong
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

; on my rarely used macbook pro, cmd=meta, option=super
(defun pd/macosx-p ()
  (string-equal "darwin" system-type))

(when (pd/macosx-p)
  (setq ns-command-modifier   'meta
        ns-alternate-modifier 'super))

; everyone loves clisp
(require 'cl)

; ido is what it all revolves around, really
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window)

; a/b, c/b; not b<2>
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

; reopen that file you just accidentally killed
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)

; better buffer menu
(require 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)

(provide 'pd/core)
