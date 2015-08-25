;; boot cask, which will handle setting up package.el
; ideally, i'd hunt down cask.el more intelligently.
; realistically, i'm always gonna `brew install cask`
; and this will work perfectly fine.
(require 'cask "/usr/local/opt/cask/cask.el")
(cask-initialize)

;; shush.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq visible-bell t
      messages-buffer-max-lines 1000)
(defalias 'yes-or-no-p 'y-or-n-p)

;; and pile your junk somewhere else.
(setq backup-directory-alist '(("." . "~/.emacs.d/.crap/backups"))
      tramp-backup-directory-alist '(("." . "~/.emacs.d/.crap/backups"))
      mc/list-file (locate-user-emacs-file "store/mc-lists.el"))

;; favor a new .el over an old .elc
(setq load-prefer-newer t)

;; religion
; 1. utf-8
; 2. spaces not tabs
; 3. no excess whitespace
; 4. files end with newlines
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default indent-tabs-mode nil)

;; stop prompting me every time i open a gemspec
(setq safe-local-variable-values
      (append '((encoding . utf-8)) safe-local-variable-values))

;; useful frame titles
(setq frame-title-format
      '(("" invocation-name "@" system-name ": ")
        (:eval (if buffer-file-name
                   (abbreviate-file-name buffer-file-name)
                 "%b"))))

;; i have an absurdly nice computer, feel free to consume RAM rabidly.
(setq gc-cons-threshold 20000000)

(provide 'pd/bootstrap)
