;; First things first.
; Liberally stealing from the emacs-starter-kit, but doing it manually
; because eventually I had no idea why things were behaving as they did.

; STFU, GTFO.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)

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
(require 'linum)

(recentf-mode t)
(ido-mode t)

(require 'project-root)
(setq project-roots
      '(("Rails project" :root-contains-files ("app" "public"))
        ("Ruby project"  :root-contains-files ("Rakefile" "lib"))
        ("emacs.d"       :root-contains-files ("init.el" "custom.el"))))

(require 'my-color-theme)
(require 'my-defuns)
(require 'my-key-bindings)
(require 'my-jumps)
(require 'my-modes)

; Settings which are obviously preferable to the defaults.
(setq column-number-mode t   ; ruler shows column number
      transient-mark-mode t  ; actually *see* what i'm selecting...
      indent-tabs-mode nil)  ; never ever ever insert an actual tab; maybe should be a mode hook instead

; Back off, hippie.
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)
