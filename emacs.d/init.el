; stfu, gtfo.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq visible-bell t)
(defalias 'yes-or-no-p 'y-or-n-p)

; Always ~/.emacs.d/ for me, but hey why not.
; aka ~/dotfiles/emacs.d, tho.
(setq emacs-dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name))
      dotfiles-dir (file-name-as-directory "~/dotfiles"))
(add-to-list 'load-path emacs-dotfiles-dir)
(add-to-list 'load-path (concat emacs-dotfiles-dir "vendor"))
(dolist (dir (directory-files (concat dotfiles-dir "vendor") 'full "[^\\.]"))
  (add-to-list 'load-path dir))

; Get emacs to stop auto-writing things to this init.el, or the cwd, &c
(setq backup-directory-alist `(("." . ,(expand-file-name (concat emacs-dotfiles-dir "backups"))))
      save-place-file (concat emacs-dotfiles-dir "places")
      custom-file (concat emacs-dotfiles-dir "custom.el"))

; Since I can't get it to gtfo, just turn off autosaving. This isn't
; 100% either, tho. Fuck this feature.
(setq auto-save-default nil)

; Gotta load the customization file manually; but don't actually do so.
; Discourage use of customize at all. My god it's ugly.
; (load custom-file 'noerror)

;; quality stuff that there's no reason to load on demand
; shipped with carbon emacs:
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'ido)

; vendor/*:
(require 'linum)
(require 'ack)
(require 'eproject)
(require 'shell-command)
(require 'jump)

(recentf-mode t)
(ido-mode t)
(ido-everywhere t)
(shell-command-completion-mode t)
(setq ido-enable-flex-matching t) ; "acs" matches "application_controller_spec"

(dolist (file '("defuns.el" "global-key-bindings.el" "jumps.el" "colors.el"))
  (load (concat emacs-dotfiles-dir file)))

(dolist (file (directory-files (concat emacs-dotfiles-dir "modes") 'full ".el$"))
  (load file))

; The Thing To Do
(prefer-coding-system 'utf-8)

; Settings which are religiously preferable to the defaults.
(setq indent-tabs-mode nil        ; has to be set again in some mode
                                  ; hooks which override this?
      column-number-mode t        ; ruler shows column number
      transient-mark-mode t       ; actually *see* what i'm selecting...
      show-trailing-whitespace t
      require-final-newline t
      uniquify-buffer-name-style 'forward) ; a/b, c/b, not b<2>

; Back off, hippie.
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)
