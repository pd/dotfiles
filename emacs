;; -*- mode: Emacs-Lisp -*-

; shush.
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq visible-bell t)
(defalias 'yes-or-no-p 'y-or-n-p)

; and go away.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      save-place-file "~/.emacs.d/places"
      custom-file "~/.emacs.d/custom.el"
      auto-save-default nil)

; always.
(require 'cl)
(require 'ido)
(require 'uniquify)
(require 'ffap)

(labels ((add-path (p)
                   (add-to-list 'load-path p)))
  (add-path "~/dotfiles/emacs.d/vendor")
  (add-path "~/dotfiles/vendor/magit")
  (add-path "~/dotfiles/vendor/smex"))

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)

; ~ in ido file/dir nav
(defun pd/ido-move-to-home ()
  (interactive)
  (ido-set-current-home)
  (ido-reread-directory))

(add-hook 'ido-setup-hook
          (lambda () (define-key ido-file-dir-completion-map
                       (kbd "~") 'pd/ido-move-to-home)))

; spaces not tabs
; always whine about whitespace.
(setq-default indent-tabs-mode nil
	      show-trailing-whitespace t)

(setq column-number-mode t
      require-final-newline t
      uniquify-buffer-name-style 'forward) ; a/b, c/b, not b<2>

; The Thing To Do
(prefer-coding-system 'utf-8)

; generics
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "<f6>") 'linum-mode)
(global-set-key (kbd "C-h a") 'apropos) ; defaults to command-apropos
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)

; C-c C-b ...: buffer management
(global-set-key (kbd "C-c C-b b") 'bury-buffer)
(global-set-key (kbd "C-c C-b l") 'list-buffers)
(global-set-key (kbd "C-c C-b r") 'rename-buffer)

; super-[left/up/down/right]
(windmove-default-keybindings 'super)

; remove a few unnecessary text movement aliases
(global-unset-key (kbd "M-<left>"))
(global-unset-key (kbd "M-<right>"))

; i hit the menu key aiming for left arrow all the time
(global-unset-key (kbd "<menu>"))

(defun pd/toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                         'fullboth)))

(global-set-key (kbd "C-M-<return>") 'pd/toggle-fullscreen)

; vi's o and O
(defun pd/insert-new-line ()
  (funcall (or (local-key-binding (kbd "<return>"))
               (key-binding (kbd "RET")))))

(defun pd/append-and-move-to-new-line ()
  "Inserts a blank line after the current one, and moves to it"
  (interactive)
  (end-of-line)
  (pd/insert-new-line))

(defun pd/prepend-and-move-to-new-line ()
  "Inserts a blank line before the current one, and move to it"
  (interactive)
  (if (= 1 (line-number-at-pos))
      (progn
        (beginning-of-buffer)
        (pd/insert-new-line)
        (beginning-of-buffer))
    (progn
      (previous-line)
      (pd/append-and-move-to-new-line))))

(global-set-key (kbd "M-<return>") 'pd/append-and-move-to-new-line)
(global-set-key (kbd "M-S-<return>") 'pd/prepend-and-move-to-new-line)

; colors
(defun pd/load-directory (path)
  (dolist (file (directory-files path 'full "\\.el\\'"))
    (load file)))

(require 'color-theme)
(setq color-theme-history-max-length t) ; unlimited
(pd/load-directory "~/dotfiles/emacs.d/themes")
(color-theme-inkpot)

; lisps
(defun pd/lisp-modes ()
  (show-paren-mode t)
  (define-key lisp-mode-shared-map (kbd "<return>") 'newline-and-indent))

(add-hook 'lisp-mode-hook 'pd/lisp-modes)
(add-hook 'emacs-lisp-mode-hook 'pd/lisp-modes)
(add-hook 'clojure-mode-hook 'pd/lisp-modes)

; haskell
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)

; ruby
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.rake\\'" . ruby-mode)
                ("Rakefile\\'" . ruby-mode))))

(autoload 'ruby-mode "ruby-mode"
  "Major mode for ruby" t)
(autoload 'run-ruby "inf-ruby"
  "Inferior mode for ruby" t)

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map (kbd "<return>") 'newline-and-indent)))

; shell
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(global-set-key (kbd "C-c s") 'shell)

; irc
(autoload 'erc "erc" "Emacs IRC Client" t)
(load "~/.erc-secrets.el") ; passwords, autojoin lists, etc

(eval-after-load 'erc
  '(progn
     (setq erc-nick "pd"
           erc-nick-uniquifier "_"
           erc-full-name "pd"
           erc-max-buffer-size 5000)
     (setq erc-log-channels-directory "~/.erc/logs")
     (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

     ; only notify about activity for actual conversation
     (setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))
     (setq erc-autojoin-channels-alist pd/erc-secrets-autojoin-alist)))

(eval-after-load 'erc-stamp
  '(progn
     ; current theme's color for my own input is awful
     (set-face-foreground 'erc-input-face "light steel blue")
     (set-face-foreground 'erc-my-nick-face "steel blue")
     (set-face-foreground 'erc-timestamp-face "grey50")))

(defun pd/irc ()
  "Connect to IRC, maybe. And prompt for each server to ensure we want to connect to it."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (dolist (server pd/erc-secrets-server-list)
      (when (y-or-n-p (concat server "? "))
        (erc :server server :password pd/erc-secrets-password)))))

(defalias 'irc 'pd/irc)

; magit
(autoload 'magit-status "magit"
  "Major mode for git interaction" t)

(global-set-key (kbd "C-M-g") 'magit-status)

(defun magit-insert-submodule-summary ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "\n" (magit-shell "git submodule summary"))))

(add-hook 'magit-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

(add-hook 'magit-log-edit-mode-hook
          (lambda ()
            (define-key magit-log-edit-map (kbd "C-M-s") 'magit-insert-submodule-summary)))

(eval-after-load 'magit
  '(progn
     (set-face-background 'magit-item-highlight "gray12")
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

; smex. should always come last.
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(setq smex-save-file "~/.emacs.d/smex.save")
(smex-auto-update 120) ; auto update after 2 minutes idle
