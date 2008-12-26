;; First things first.
; Liberally stealing from the emacs-starter-kit, but doing it manually
; because eventually I had no idea why things were behaving as they did.

; STFU, GTFO.
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(menu-bar-mode -1)

; Always ~/.emacs.d/ for me, but hey why not.
(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

; Make sure Custom doesn't write to this friggin file.
(setq custom-file (concat dotfiles-dir "/custom.el"))
(load custom-file 'noerror)

; Move backup files somewhere else
(setq backup-directory-alist `(("." . ,(expand-file-name
					(concat dotfiles-dir "backups")))))

; Libraries the emacs-starter-kit assures me I always want. Cursory
; overview suggests it's generally the case.
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

(require 'ido)
(ido-mode t)

; Can't believe how awkward good line numbering is in Emacs.
(require 'linum)
(global-set-key (kbd "<f6>") 'linum-mode)

; Settings which are obviously preferable to the defaults.
(setq column-number-mode t)  ; ruler shows column number
(setq transient-mark-mode t) ; actually *see* what i'm selecting...

; color-theme ships with Carbon emacs.
(require 'color-theme)
(setq color-theme-is-cumulative nil)
(color-theme-initialize)
(color-theme-deep-blue)

; Keybindings to mimic OS X and readline in my terminal
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-SPC") 'set-mark-command) ; Quicksilver is C-SPC

; Liked this from emacs-starter-kit
(global-set-key (kbd "C-c v") 'eval-buffer)

;;;; Things only to be loaded on demand

; ruby mode
(autoload 'ruby-mode "ruby-mode"
  "Major mode for ruby" t)
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-hook 'ruby-mode-hook
 '(lambda ()
    (require 'ruby-electric)))

(autoload 'haml-mode "haml-mode"
  "Major mode for HAML files" t)
(autoload 'sass-mode "sass-mode"
  "Major mode for Sass files" t)
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode))

(autoload 'js2-mode "js2-mode"
  "Major mode for JS files" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(autoload 'yaml-mode "yaml-mode"
  "Major mode for YAML files" t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git" t)
