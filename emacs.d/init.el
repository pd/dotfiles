;; First things first.
; Liberally stealing from the emacs-starter-kit, but doing it manually
; because eventually I had no idea why things were behaving as they did.

(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

; Ships with Carbon emacs.
(require 'color-theme)
(setq color-theme-is-cumulative nil)
(color-theme-initialize)
(color-theme-deep-blue)

; Keybindings to mimic OS X and readline in my terminal
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-SPC") 'set-mark-command) ; Quicksilver is C-SPC

;;;; Things only to be loaded on demand
; ruby mode
(autoload 'ruby-mode "ruby-mode"
  "Minor mode for ruby" t)
(add-to-list 'auto-mode-alist '("\\.(rb|rake)$" . ruby-mode))

(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git" t)

; haml / sass
(autoload 'haml-mode "haml-mode"
  "Minor mode for HAML files" t)
(autoload 'sass-mode "sass-mode"
  "Minor mode for Sass files" t)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
