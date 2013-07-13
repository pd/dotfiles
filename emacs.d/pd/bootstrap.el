;; Prep package.el
(require 'package)
(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

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
      custom-file "~/.emacs.d/.crap/custom.el"
      mc/list-file (expand-file-name "store/mc-lists.el" user-emacs-directory))

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

(provide 'pd/bootstrap)
