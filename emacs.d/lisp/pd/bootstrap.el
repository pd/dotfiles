;; boot cask, which will handle setting up package.el
(when (file-directory-p (expand-file-name "~/.cask"))
  (add-to-list 'load-path (expand-file-name "~/.cask")))
(when (file-directory-p "/usr/local/opt/cask")
  (add-to-list 'load-path "/usr/local/opt/cask"))
(require 'cask)
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
      mc/list-file (locate-user-emacs-file "store/mc-lists.el")
      nsm-settings-file (locate-user-emacs-file ".crap/network-security.data"))

;; favor a new .el over an old .elc
(setq load-prefer-newer t)

;; disable lockfiles
; i use one big emacs, they're never actually helpful. and the stray files
; constantly cause issues for tools that don't ignore dotfiles.
(setq create-lockfiles nil)

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
