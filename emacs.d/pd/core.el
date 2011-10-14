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
(setq backup-directory-alist '(("." . "~/.emacs.d/.crap/backups"))
      tramp-backup-directory-alist '(("." . "~/.emacs.d/.crap/backups"))
      save-place-file "~/.emacs.d/.crap/places"
      custom-file "~/.emacs.d/.crap/custom.el"
      ac-comphist-file "~/.emacs.d/.crap/ac-comphist.dat"
      save-visited-files-location "~/.emacs.d/.crap/emacs-visited-files"
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

(defun pd/macosx-p ()
  "t if on a darwin system"
  (string-equal "darwin" system-type))

; on mac, cmd=meta, option=super
(when (pd/macosx-p)
  (setq ns-command-modifier   'meta
        ns-alternate-modifier 'super))

; gank the $PATH from a login shell, in case I launched from the dock
; http://stackoverflow.com/questions/2266905/emacs-is-ignoring-my-path-when-it-runs-a-compile-command
(when (pd/macosx-p)
  (let ((path (pd/login-shell-path)))
    (setenv "PATH" (mapconcat 'identity path path-separator))
    (setq exec-path path)))

; useful frame titles
(setq frame-title-format '(("" invocation-name "@" system-name ": ")
                           (:eval (if buffer-file-name
                                      (abbreviate-file-name buffer-file-name)
                                    "%b"))))

; and i use emacs over x11 forwarding, so show the hostname in the modeline
(let ((pos (memq 'mode-line-modes mode-line-format)))
  (setcdr pos (cons 'system-name (cdr pos))))

; everyone loves clisp
(require 'cl)

; ido is what it all revolves around, really
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window
      ido-use-virtual-buffers t)

; a/b, c/b; not b<2>
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

; reopen that file you just accidentally killed
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)

; better buffer menu
(require 'ibuffer)
(setq ibuffer-default-sorting-mode 'filename/process)

; winner-mode: undo/redo window state changes
; holy christ how have i not been using this forever?
(require 'winner)
(winner-mode 1)

(provide 'pd/core)
