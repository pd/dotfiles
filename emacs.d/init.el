;; First things first.
; Liberally stealing from the emacs-starter-kit, but doing it manually
; because eventually I had no idea why things were behaving as they did.

; STFU, GTFO.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)

; Always ~/.emacs.d/ for me, but hey why not.
(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

; Get emacs to stop auto-writing things to this init.el, or the cwd, &c
(setq backup-directory-alist `(("." . ,(expand-file-name (concat dotfiles-dir "backups"))))
      save-place-file (concat dotfiles-dir "/places")
      custom-file (concat dotfiles-dir "/custom.el"))

; Gotta load the customization file manually; but don't actually do so.
; Discourage use of customize at all. My god it's ugly.
; (load custom-file 'noerror)

; Libraries the emacs-starter-kit assures me I always want. Cursory
; overview suggests it's generally the case.
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'ido)
(recentf-mode t)
(ido-mode t)

; Can't believe how awkward good line numbering is in Emacs.
(require 'linum)
(global-set-key (kbd "<f6>") 'linum-mode)

; Settings which are obviously preferable to the defaults.
(setq column-number-mode t   ; ruler shows column number
      transient-mark-mode t  ; actually *see* what i'm selecting...
      indent-tabs-mode nil)  ; never ever ever insert an actual tab; maybe should be a mode hook instead

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

; Bindings from emacs-starter-kit that were reasonable enough
(global-set-key (kbd "C-c v") 'eval-buffer)
(global-set-key (kbd "C-h a") 'apropos) ; defaults to command-apropos
(global-set-key (kbd "C-/") 'hippie-expand) ; keep M-/ around for now, I suppose

(windmove-default-keybindings) ; S-<dir> moves to the window in that direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2)))

; Also from emacs-starter-kit
(defun cleanup-buffer ()
  "Reindent, untabify, and delete trailing whitespace from this buffer"
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))
(global-set-key (kbd "C-c c") 'cleanup-buffer)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

; Back off, hippie expand.
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)

;;;; Things only to be loaded on demand

(autoload 'ruby-mode "ruby-mode" "Major mode for ruby" t)
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-hook 'ruby-mode-hook
          '(lambda ()
             (require 'ruby-electric)))

(autoload 'haml-mode "haml-mode" "Major mode for HAML files" t)
(autoload 'sass-mode "sass-mode" "Major mode for Sass files" t)
(add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode))

(autoload 'js2-mode "js2-mode" "Major mode for JS files" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(autoload 'yaml-mode "yaml-mode" "Major mode for YAML files" t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git" t)
