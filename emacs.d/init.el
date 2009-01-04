; stfu, gtfo.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq visible-bell t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq auto-save-default nil) ; no more "#foo#" files. ty fkn god.

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
(require 'linum)
(require 'ack)

(recentf-mode t)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t) ; "acs" matches "application_controller_spec"

(require 'project-root)
(setq project-roots
      '(("Rails project" :root-contains-files ("config/environment.rb"))
        ("Ruby project"  :root-contains-files ("Rakefile" "lib"))
        ("emacs.d"       :root-contains-files ("init.el" "custom.el"))))

(dolist (file '("defuns.el" "global-key-bindings.el" "jumps.el" "colors.el"))
  (load (concat emacs-dotfiles-dir file)))

(dolist (file (directory-files (concat emacs-dotfiles-dir "modes") 'full ".el$"))
  (load file))

; The Thing To Do
(prefer-coding-system 'utf-8)

; Settings which are obviously preferable to the defaults.
(setq column-number-mode t        ; ruler shows column number
      transient-mark-mode t       ; actually *see* what i'm selecting...
      indent-tabs-mode nil        ; don't insert an actual tab; move to mode hooks?
      show-trailing-whitespace t
      require-final-newline t)

; Back off, hippie.
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)
