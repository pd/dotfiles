; shush.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq visible-bell t)
(setq messages-buffer-max-lines 1000)
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
;   3. files end with newlines
(setq-default indent-tabs-mode nil
              show-trailing-whitespace t
              require-final-newline t)

; utf-8 seems the least wrong
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

; useful frame titles
(setq frame-title-format '(("" invocation-name "@" system-name ": ")
                           (:eval (if buffer-file-name
                                      (abbreviate-file-name buffer-file-name)
                                    "%b"))))

; and i use emacs over x11 forwarding, so show the hostname in the modeline
(let ((pos (memq 'mode-line-modes mode-line-format)))
  (setcdr pos (cons 'system-name (cdr pos))))

(provide 'pd/core)
