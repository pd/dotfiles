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
(require 'recentf)

(labels ((add-path (p)
                   (add-to-list 'load-path p)))
  (add-path "~/.emacs.d/vendor")
  (add-path "~/dotfiles/vendor/magit")
  (add-path "~/dotfiles/vendor/smex")
  (add-path "~/dotfiles/vendor/emacs_chrome/servers"))

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(recentf-mode t)
(setq ibuffer-default-sorting-mode 'major-mode)

; load elpa
(when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

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
      uniquify-buffer-name-style 'forward ; a/b, c/b, not b<2>
      woman-use-own-frame nil)

; The Thing To Do
(prefer-coding-system 'utf-8)

; generics
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "<f6>") 'linum-mode)
(global-set-key (kbd "C-h a") 'apropos) ; defaults to command-apropos
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c m") 'woman)
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)

; C-c C-b ...: buffer management
(global-set-key (kbd "C-c C-b b") 'bury-buffer)
(global-set-key (kbd "C-c C-b r") 'rename-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

; super-[left/up/down/right]: window focus switching
(windmove-default-keybindings 'super)

; M-S-[left/up/down/right]: window resizing
; the directions unfortunately don't always make sense in context
(global-set-key (kbd "M-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-S-<up>") 'enlarge-window)
(global-set-key (kbd "M-S-<down>") 'shrink-window)
(global-set-key (kbd "M-S-<right>") 'enlarge-window-horizontally)

; remove a few unnecessary text movement aliases
(global-unset-key (kbd "M-<left>"))
(global-unset-key (kbd "M-<right>"))

; i hit the menu key aiming for left arrow all the time
(global-unset-key (kbd "<menu>"))

; ido access to recentf-list
; ganked from emacs-starter-kit
(defun pd/recentf-ido-find-file ()
  (interactive)
  (let ((file (ido-completing-read "Find recent: " recentf-list nil t)))
    (when file
      (find-file file))))
(global-set-key (kbd "C-x f") 'pd/recentf-ido-find-file)

; colors
(defun pd/load-directory (path)
  (dolist (file (directory-files path 'full "\\.el\\'"))
    (load file)))

(require 'color-theme)
(setq color-theme-history-max-length t) ; unlimited
(pd/load-directory "~/.emacs.d/vendor/themes")
(color-theme-inkpot)

; vi's o and O
(defun pd/insert-newline ()
  (funcall (or (local-key-binding (kbd "<return>"))
               (key-binding (kbd "RET")))))

(defun pd/append-and-move-to-new-line ()
  "Inserts a blank line after the current one, and moves to it"
  (interactive)
  (end-of-line)
  (pd/insert-newline))

(defun pd/prepend-and-move-to-new-line ()
  "Inserts a blank line before the current one, and moves to it"
  (interactive)
  (if (= 1 (line-number-at-pos))
      (progn
        (beginning-of-buffer)
        (pd/insert-newline)
        (beginning-of-buffer))
    (progn
      (previous-line)
      (pd/append-and-move-to-new-line))))

(global-set-key (kbd "M-<return>") 'pd/append-and-move-to-new-line)
(global-set-key (kbd "M-S-<return>") 'pd/prepend-and-move-to-new-line)

; hippie-expand: don't complete a giant lisp list if we can just complete
; a symbol first. redefining all ordering because it's clearer than
; doing lots of list manipulation.
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-dabbrev-all-buffers
        try-expand-list
        try-expand-line))

; lisps
(defun pd/lisp-modes ()
  (show-paren-mode t)
  (paredit-mode)
  (define-key lisp-mode-shared-map (kbd "<return>") 'newline-and-indent))

(defun pd/lein-swank ()
  (interactive)
  (let ((default-directory (locate-dominating-file (buffer-file-name) "project.clj"))
        (explicit-shell-file-name "lein")
        (explicit-lein-args '("swank")))
    (when default-directory
      (shell "*lein-swank*"))))

(add-hook 'lisp-mode-hook 'pd/lisp-modes)
(add-hook 'emacs-lisp-mode-hook 'pd/lisp-modes)
(add-hook 'clojure-mode-hook 'pd/lisp-modes)
(add-hook 'slime-repl-mode-hook
          (lambda ()
            (paredit-mode)))

; javascript
(autoload 'espresso-mode "espresso" "Major mode for javascript" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . espresso-mode))
(setq espresso-indent-level 2)

; haskell
(if (load "haskell-site-file" 'noerror)
    (progn
      (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
      (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
      (add-hook 'haskell-mode-hook 'turn-on-font-lock))
  (message "Haskell mode unavailable"))

; ruby
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.rake\\'" . ruby-mode)
                ("Rakefile\\'" . ruby-mode))))

(autoload 'ruby-mode "ruby-mode" "Major mode for ruby" t)
(autoload 'run-ruby "inf-ruby" "Inferior mode for ruby" t)

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map (kbd "<return>") 'newline-and-indent)))

; haml/sass
(autoload 'haml-mode "haml-mode" "Major mode for haml" t)
(autoload 'sass-mode "sass-mode" "Major mode for sass" t)
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.haml\\'" . haml-mode)
                ("\\.sass\\'" . sass-mode))))

; shell
(require 'ansi-color)
(add-hook 'shell-mode-hook
          (lambda ()
            (ansi-color-for-comint-mode-on)
            (setq show-trailing-whitespace nil)))
(global-set-key (kbd "C-c s") 'shell)

; irc
(autoload 'erc "erc" "Emacs IRC Client" t)
(load "~/.erc-secrets.el" 'noerror 'nomessage) ; passwords, autojoin lists, etc

(eval-after-load 'erc
  '(progn
     (setq erc-nick "pd"
           erc-nick-uniquifier "_"
           erc-full-name "pd"
           erc-max-buffer-size 5000
           erc-join-buffer 'window-noselect)
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
  "Connect to IRC, maybe."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (dolist (server pd/erc-secrets-server-list)
      (when (y-or-n-p (concat server "? "))
        (erc :server server :password pd/erc-secrets-password)))))

(defalias 'irc 'pd/irc)

; twitter
(autoload 'twit "twittering-mode" "Major mode for twitter interaction" t)
(defalias 'tweet 'twit)
(add-hook 'twittering-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-c C-l"))))

; magit
(autoload 'magit-status "magit" "Major mode for git interaction" t)
(global-set-key (kbd "C-M-g") 'magit-status)

(defun pd/magit-insert-submodule-summary ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "\n" (magit-shell "git submodule summary"))))

(add-hook 'magit-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

(add-hook 'magit-log-edit-mode-hook
          (lambda ()
            (define-key magit-log-edit-map (kbd "C-M-s")
              'pd/magit-insert-submodule-summary)))

(eval-after-load 'magit
  '(progn
     (set-face-background 'magit-item-highlight "gray12")
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

; tramp
(setq tramp-default-method "ssh")

; ack
(eval-after-load 'full-ack
  '(progn
     (setq ack-ignore-case t
           ack-arguments (list "-a"))))

; chrome is my default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

; boot emacs chrome server if this is emacsd
(if (and (daemonp) (locate-library "edit-server"))
    (progn
      (require 'edit-server)
      (edit-server-start)))

; smex. should always come last.
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(setq smex-save-file "~/.emacs.d/smex.save")
(smex-auto-update 120) ; auto update after 2 minutes idle
