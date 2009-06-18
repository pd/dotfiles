; why so hostile
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq visible-bell t)
(defalias 'yes-or-no-p 'y-or-n-p)

; ...
(setq emacs-dotfiles-dir (file-name-as-directory "~/dotfiles/emacs.d")
      dotfiles-dir (file-name-as-directory "~/dotfiles"))

; Add emacs.d, emacs.d/vendor, and dotfiles/vendor/* to the load path
(add-to-list 'load-path emacs-dotfiles-dir)
(add-to-list 'load-path (concat emacs-dotfiles-dir "vendor"))
(dolist (dir (directory-files (concat dotfiles-dir "vendor") 'full "[^\\.]"))
  (add-to-list 'load-path dir))

; Get emacs to stop auto-writing things to this init.el, or the cwd, &c
(setq backup-directory-alist `(("." . ,(expand-file-name (concat emacs-dotfiles-dir "backups"))))
      save-place-file (concat emacs-dotfiles-dir "places")
      custom-file (concat emacs-dotfiles-dir "custom.el")
      auto-save-default nil)

;; elpa
(let ((package-el (concat emacs-dotfiles-dir "elpa/package.el")))
  (when (file-readable-p package-el)
    (load package-el)
    (package-initialize)))

;; quality stuff that there's no reason to load on demand
; shipped with carbon emacs:
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'ido)

; vendor/*:
(require 'linum)
(require 'ack)
(require 'eproject)
(require 'shell-command)
(require 'keats)
(require 'keats-interactive)
(require 'smex)

;; turn all that shit on
(ido-mode t)
(ido-everywhere t)
(shell-command-completion-mode t)
(setq ido-enable-flex-matching t) ; "acs" matches "application_controller_spec"
(setq ack-guess-type t)
(add-to-list 'ack-mode-type-map '((ruby-mode) . "ruby") 'append)
(setq keats-file     (concat emacs-dotfiles-dir "keats")
      smex-save-file (concat emacs-dotfiles-dir "smex.save"))
(keats-mode t)
(smex-auto-update 120) ; auto update after 2 mins idle


(dolist (file '("defuns.el" "global-key-bindings.el" "jumps.el" "colors.el"))
  (load (concat emacs-dotfiles-dir file)))

(dolist (file (directory-files (concat emacs-dotfiles-dir "modes") 'full ".el$"))
  (load file))

; The Thing To Do
(prefer-coding-system 'utf-8)

; Settings which are religiously preferable to the defaults.
(setq-default indent-tabs-mode nil
              show-trailing-whitespace t)

; And so on
(setq column-number-mode t        ; ruler shows column number
      transient-mark-mode t       ; actually *see* what i'm selecting...
      require-final-newline t
      uniquify-buffer-name-style 'forward ; a/b, c/b, not b<2>
      tramp-default-method "ssh"
      tramp-shell-prompt-pattern "^$ ")

; Tell hippie expand not to expand an entire line
(delete 'try-expand-line hippie-expand-try-functions-list)

; Hullo emacsclient
(server-start)

; very last please
(smex-initialize)

