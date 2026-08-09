;; -*- lexical-binding: t; -*-

;; init use-package
(setopt package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("elpa" . "https://elpa.gnu.org/packages/"))
        use-package-verbose t
        use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))

;; my :fmt and :leader use-package kws
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'pd/use-package-fmt)
(require 'pd/use-package-leader)

;; tuck most things away in emacs.d/{etc,var}
(use-package no-littering
  :config
  (no-littering-theme-backups)
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file))

;; simmer down
(horizontal-scroll-bar-mode -1)
(blink-cursor-mode -1)
(setopt inhibit-startup-screen t
        visible-bell nil
        messages-buffer-max-lines 1000
        create-lockfiles nil
        read-extended-command-predicate #'command-completion-default-include-p
        ring-bell-function 'ignore
        use-short-answers t
        redisplay-skip-fontification-on-input t
        cursor-in-non-selected-windows nil
        highlight-nonselected-windows nil
        bidi-display-reordering 'left-to-right
        bidi-paragraph-direction 'left-to-right
        bidi-inhibit-bpa t
        kill-do-not-save-duplicates t)
(use-package diminish)

;; but still simmer
(column-number-mode +1)
(show-paren-mode +1)
(setopt read-process-output-max (* 4 1024 1024)
        ;; keep clipboard contents when killing text over it
        save-interprogram-paste-before-kill t)

;; religion
;; 1. utf-8
;; 2. spaces not tabs
;; 3. no excess whitespace
;; 4. files end with newlines
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setopt indent-tabs-mode nil
        tab-width 4
        tab-always-indent 'complete
        require-final-newline t)

;; sane env
(setenv "EDITOR" "emacsclient")
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize)
  :if (or (daemonp)
          (memq window-system '(ns pgtk))))

;; decent theme
(use-package kanagawa-themes
  :custom-face
  (comint-highlight-prompt
   ((t (:inherit minibuffer-prompt
                 :background unspecified
                 :foreground "#9cabca"))))
  :config
  (load-theme 'kanagawa-wave t))

(use-package doom-modeline
  :init (doom-modeline-mode +1))

;; unstable.emacs 31
(when (eq system-type 'darwin)
  (setopt ns-command-modifier      'meta
          ns-alternate-modifier    'super
          ns-function-modifier     'hyper
          ns-use-native-fullscreen nil
          dired-use-ls-dired       nil)

  (add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font"))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . nil))

  ;; no i do not want to print
  (unbind-key "s-p"))

;; unstable.emacs31-pgtk
(when (eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(undecorated . t))
  (add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font-10")))

;; bought into the whole vertico ecosystem
(use-package vertico
  :init (vertico-mode)
  :bind
  (:map vertico-map
        ("C-M-n" . vertico-next-group)
        ("C-M-p" . vertico-previous-group)))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package savehist
  :ensure nil
  :init (savehist-mode))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :commands
  (consult-buffer consult-focus-lines consult-goto-line consult-imenu consult-ripgrep)
  :bind
  (("C-x b"      . consult-buffer)
   ("<leader>cb" . consult-buffer)
   ("<leader>cg" . consult-ripgrep)
   ("<leader>ce" . consult-flymake)
   ("<leader>ci" . consult-imenu)
   ("<leader>cl" . consult-line)
   ("<leader>cL" . consult-goto-line)
   ("<leader>cm" . consult-focus-lines)) ; "matching"
  :custom
  (consult-narrow-key "<")
  :init
  (setopt xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
  :config
  ;; these previews open files, so let the input settle first. in-buffer
  ;; previews (consult-line, -imenu, -flymake) stay on every keystroke.
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

(use-package consult-dir
  :after consult
  :bind
  ("<leader>cd" . consult-dir))

(use-package consult-project-extra
  :after consult
  :bind
  ("<leader>cp" . consult-project-extra-find))

(use-package embark
  :bind
  (("s-."   . embark-act)
   ("s-;"   . embark-dwim)
   ("C-h b" . embark-bindings))

  :custom
  (embark-indicators '(embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; https://github.com/oantolin/embark/wiki/Additional-Configuration/e1287536333d73c0be512ea65b366f5e8476257e#use-which-key-like-a-key-menu-prompt
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

(use-package embark-consult
  :after (embark consult))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-pcm-leading-wildcard t)
  (completion-category-overrides
   '((file         (styles basic partial-completion))
     (buffer       (styles orderless basic))
     (project-file (styles orderless basic))
     (eglot-capf   (styles orderless basic)))))

;; condense the 900 ephemeral windows into a single one i can toggle
;; open/closed at will
(use-package popper
  :custom
  (popper-reference-buffers '("\\*Messages\\*"
                              "\\*Backtrace\\*"
                              help-mode
                              compilation-mode
                              ("\\*Warnings\\*" . hide)))
  :init
  (popper-mode +1)
  (popper-echo-mode +1)
  :bind
  (("s-`" . popper-toggle)
   ("M-`" . popper-cycle)))

;; vim
(use-package undo-fu
  :init
  (unbind-key "C-z")
  :bind
  (("C-z"   . undo-fu-only-undo)
   ("C-S-z" . undo-fu-only-redo)))

(use-package evil
  :demand t
  :bind
  (("<escape>" . keyboard-escape-quit))
  :custom
  (evil-search-module 'evil-search)
  (evil-undo-system 'undo-fu)
  (evil-want-keybinding nil)

  ;; RET in repls should be submit in both insert and normal state;
  ;; use S-RET to force a newline
  (evil-collection-binding-overrides
   '((repl-submit :state (insert normal))
     (repl-newline :enabled nil)))

  :init
  (evil-mode)
  :config
  (evil-set-leader '(normal visual motion) (kbd "SPC"))
  (evil-ex-define-cmd "q" 'kill-current-buffer)
  (evil-ex-define-cmd "wq" (lambda () (save-buffer) (kill-current-buffer)))
  (evil-set-initial-state 'git-commit-mode 'insert))

(use-package anzu
  :after evil
  :diminish
  :init (global-anzu-mode +1))

(use-package evil-anzu
  :after (evil anzu))

(use-package evil-cleverparens
  :after (evil smartparens)
  :diminish
  :hook
  ((lisp-mode emacs-lisp-mode inferior-emacs-lisp-mode) . evil-cleverparens-mode)
  :custom
  (evil-cleverparens-use-additional-movement-keys nil)
  :config
  (require 'evil-cleverparens-text-objects))

(use-package evil-collection ;; https://github.com/emacs-evil/evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :diminish
  :init (evil-commentary-mode))

(use-package evil-lion
  :after evil
  :init (evil-lion-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode +1))

(use-package evil-textobj-line)
(use-package evil-textobj-syntax)

;; mostly intuitive evil multicursor. evil-mc never stays in my
;; head for whatever reason. C-n or C-up/down as the main entrypoint.
(use-package evim
  :after evil
  :config (evim-setup-global-keys))

;; qol
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

(use-package deadgrep
  :bind
  ("s-g" . deadgrep)
  :custom
  (deadgrep-project-root-function 'pd/deadgrep-project-root)
  :config
  (defun pd/deadgrep-project-root ()
    "deadgrep current dir in dired-mode"
    (if (eq major-mode 'dired-mode) default-directory
      (deadgrep--project-root))))

(use-package dired
  :ensure nil
  :leader dired-mode-map)

(use-package envrc
  :init (envrc-global-mode)
  :custom
  (envrc-on-lighter '(" " (:propertize "direnv" face envrc-mode-line-on-face)))
  (envrc-none-lighter '(""))
  (envrc-error-lighter '(" " (:propertize "direnv" face envrc-mode-line-error-face))))

(use-package expreg
  :bind
  (("C-=" . expreg-expand)
   ("C--" . expreg-contract)))

(use-package git-link
  :custom
  (git-link-use-commit t))

(use-package magit
  :leader magit-mode-map
  :bind
  (("C-x g" . magit-status))
  :custom
  (magit-save-repository-buffers 'dontask))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-completion
  :after nerd-icons
  :config (nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :after (nerd-icons corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :diminish
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 250)
  :init (recentf-mode)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package reformatter)

(use-package smartparens
  :diminish
  :config (require 'smartparens-config)
  :hook ((emacs-lisp-mode inferior-emacs-lisp-mode lisp-mode) . smartparens-mode))

(use-package subword
  :ensure nil
  :diminish)

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package which-key
  :ensure nil
  :init (which-key-mode)
  :diminish
  :custom
  (which-key-idle-delay 0.8))

(use-package windmove
  :ensure nil
  :bind (("s-h" . windmove-left)
         ("s-j" . windmove-down)
         ("s-k" . windmove-up)
         ("s-l" . windmove-right)))

(use-package winner
  :ensure nil
  :init (winner-mode))

;; prog
(use-package prog-mode
  :ensure nil
  :config
  (dolist (mode '(display-line-numbers-mode
                  hl-line-mode
                  subword-mode))
    (add-hook 'prog-mode-hook mode)))

(use-package cue-mode)

(use-package dockerfile-mode)

(use-package eldoc
  :ensure nil
  :diminish
  :custom
  ;; :diminish just isn't working, no idea why, so kill it with fire
  (eldoc-minor-mode-string nil))

(use-package enh-ruby-mode
  :mode "\\.rb\\'"
  :custom
  (enh-ruby-preserve-indent-in-heredocs t))

(use-package inf-ruby)

(use-package go-ts-mode
  :ensure nil
  :fmt (:program "goimports")
  :custom
  (go-ts-mode-indent-offset 4))

(use-package jsonnet-mode
  :fmt (:program "jsonnetfmt" :args ("-")))

(use-package just-ts-mode)

(use-package markdown-ts-mode
  :ensure nil
  :mode "\\.md\\'")

(use-package grip-mode
  :custom
  (grip-command 'go-grip)
  :bind
  (:map markdown-ts-mode-map
        ("C-c g" . grip-mode)))

(use-package nix-mode
  :fmt (:program "nixfmt")
  :config
  (defun pd/nix-repl-from-project-root (orig &rest args)
    (let ((root (project-root (project-current))))
      (let ((default-directory (or root default-directory)))
        (apply orig args))))
  (advice-add 'nix-repl :around #'pd/nix-repl-from-project-root))

(use-package project
  :ensure nil
  :custom
  ;; treat Cargo.toml as a "root" so eglot launches rust-analyzer from
  ;; dotfiles/pkgs/waybar-pd instead of dotfiles root
  (project-vc-extra-root-markers '("Cargo.toml"))

  :config
  ;; https://github.com/golang/tools/blob/8d38122b0b1a9991f490aa06b7bfca7b4140bdad/gopls/doc/emacs.md#configuring-eglot
  ;; so eglot starts LSP in a reasonable spot when jumping into ~/go/pkg/... et al
  (defun project-find-go-module (dir)
    (when-let* ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'project-find-go-module))

(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'")

(use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 2))

(use-package sops
  :init
  (define-minor-mode pd/sops-edit-mode "sops-mode bindings"
    :lighter ""
    :keymap (make-sparse-keymap))
  (defun pd/maybe-sops-edit-mode ()
    (when (or (sops--is-sops-file) (equal sops--status "decrypted"))
      (pd/sops-edit-mode +1)))
  :bind
  (("C-c C-d" . sops-edit-file)
   ("C-c C-c" . sops-save-file)
   ("C-c C-k" . sops-cancel))
  :config
  (global-sops-mode +1)
  (add-hook 'sops-mode-hook 'pd/maybe-sops-edit-mode))

(use-package terraform-mode
  :custom
  (terraform-format-on-save nil)
  :fmt (:program (or (executable-find "tofu") (executable-find "terraform"))
        :args ("fmt" "-no-color" "-")))

(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

(use-package whitespace-cleanup-mode
  :diminish
  :hook (prog-mode markdown-mode)
  :custom
  (whitespace-cleanup-mode-only-if-initially-clean nil))

(use-package yaml-mode)

(use-package zig-ts-mode
  :fmt (:program "zig" :args ("fmt" "--stdin")))

;; ide
(use-package cape
  :bind
  ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package corfu
  :bind
  (:map corfu-map
        ("C-SPC" . corfu-insert-separator)
        ("M-RET" . corfu-insert))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-preselect 'directory)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  ;; complete only with TAB, not RET
  (keymap-unset corfu-map "RET"))

(use-package dape
  :preface
  (setq dape-key-prefix nil)
  :custom
  (dape-buffer-window-arrangement 'right)
  :bind
  (("<leader>dd" . dape)
   ("<leader>di" . dape-info)
   ("<leader>dq" . dape-quit)
   ("<leader>dQ" . dape-disconnect-quit)
   ("<leader>dr" . dape-restart)

   ("<leader>db" . dape-breakpoint-toggle)
   ("<leader>dB" . dape-breakpoint-remove-all)

   ("<leader>dc" . dape-continue)
   ("<leader>dn" . dape-next)
   ("<leader>do" . dape-step-out)
   ("<leader>ds" . dape-step-in)
   ("<leader>dp" . dape-pause)
   ("<leader>dw" . dape-watch-dwim)
   ("<leader>dx" . dape-evaluate-expression)))

(use-package eglot
  :ensure nil
  :hook
  ((go-ts-mode nix-mode rust-ts-mode typescript-ts-mode tsx-ts-mode zig-ts-mode) . eglot-ensure)
  :custom
  (eglot-documentation-renderer 'markdown-ts-view-mode)
  (eglot-code-action-indications nil)
  :bind
  (("<leader>la" . eglot-code-actions)
   ("<leader>lf" . eglot-format-buffer)
   ("<leader>lr" . eglot-rename)
   ("<leader>lx" . eglot-shutdown)
   ("<leader>lX" . eglot-shutdown-all)
   ("<leader>lz" . eglot-reconnect))
  :config
  (add-to-list 'eglot-server-programs '(zig-ts-mode . ("zls"))))

(use-package flymake
  :ensure nil
  :custom
  (flymake-mode-line-lighter "fm"))

(use-package ibuffer :ensure nil)

(use-package ibuffer-vc
  :after ibuffer
  :custom
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-show-empty-filter-groups nil)
  :config
  (defun pd/prepare-ibuffer ()
    (ibuffer-auto-mode +1)
    (ibuffer-vc-set-filter-groups-by-vc-root))
  (add-hook 'ibuffer-mode-hook 'pd/prepare-ibuffer))

(use-package tramp
  :ensure nil
  :autoload (tramp-parse-sconfig))

(use-package treesit
  :ensure nil
  :custom
  (treesit-enabled-modes t))


;; shell
(use-package ghostel
  :custom
  (ghostel-kill-buffer-on-exit nil)
  (ghostel-module-auto-install nil)
  :config
  (setopt ghostel-keymap-exceptions
          (append ghostel-keymap-exceptions '("M-'"))))

(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

(use-package pd/term
  :ensure nil
  :bind
  (("M-'"   . pd/consult-term)
   ("M-s-'" . pd/term-on)))


;; junkdrawer
(defun pd/reload-buffer ()
  "Kill the current buffer and immediately reload it without moving point."
  (interactive)
  (let ((path (buffer-file-name))
        (point (point)))
    (kill-buffer)
    (find-file path)
    (goto-char point)))

(defun pd/find-init.el ()
  "find-file init.el"
  (interactive)
  (find-file (expand-file-name user-init-file)))

(defun pd/comment-dwim (arg)
  "If the region is active (`mark-active') and `transient-mark-mode'
is on, lets `comment-dwim' do its thing.
If not, `comment-dwim' doesn't DWIM at all. Instead, comment or
uncomment the current line."
  (interactive "*P")
  (if (and mark-active transient-mark-mode)
      (comment-dwim arg)
    (save-excursion
      (back-to-indentation)
      (let ((beg (point)))
        (end-of-line)
        (comment-or-uncomment-region beg (point))))))

;; the end
(use-package emacs
  :ensure nil
  :init
  (unbind-key "M-t")

  :bind
  (("<leader>bz" . pd/reload-buffer)
   ("<leader>bb" . ibuffer)

   ;; misc
   ("M-;"     . pd/comment-dwim)
   ("C-x C-b" . ibuffer)
   ("C-x C-d" . dired)
   ("C-x d"   . dired)
   ("C-c w"   . delete-trailing-whitespace)
   ("C-c ="   . align-regexp)
   ("C-M-+"   . text-scale-increase)
   ("C-M-_"   . text-scale-decrease)

   ;; jumps
   ("<leader>je" . consult-flymake) ; also ce, but i think of it as a "jump" often
   ("<leader>jf" . find-function)
   ("<leader>ji" . pd/find-init.el)
   ("<leader>jl" . find-library)
   ("<leader>jk" . find-function-on-key)
   ("<leader>jp" . project-find-file)
   ("<leader>jv" . find-variable)

   ;; searchreplace
   ("<leader>rr" . replace-regexp)
   ("<leader>rs" . replace-string)

   ;; transpositions
   ("<leader>tc" . transpose-chars)
   ("<leader>tw" . transpose-words)
   ("<leader>tl" . transpose-lines)
   ("<leader>ts" . transpose-sexps)

   ;; repls
   ("<leader>xe" . ielm)
   ("<leader>xn" . nix-repl)
   ("<leader>xr" . inf-ruby)

   ;; other window plz
   ("<leader>wo" . other-window-prefix)))

;; add private to load-path iff it's available
(let ((private-emacs (expand-file-name "~/dotfiles/private/emacs.d/")))
  (when (file-directory-p private-emacs)
    (add-to-list 'load-path private-emacs)
    (require 'pd/work)))
