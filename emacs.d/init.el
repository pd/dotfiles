(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

;; bootstrap package/use-package
(package-initialize)
(setq use-package-always-ensure t
      use-package-verbose t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; tuck most things away in emacs.d/{etc,var}
(use-package no-littering
  :config
  (setq custom-file (expand-file-name "etc/custom.el" user-emacs-directory))
  (load custom-file)
  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; i've got plenty of ram
(setq gc-cons-threshold (* 1024 1024 32))

;; simmer down
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(blink-cursor-mode -1)
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      visible-bell nil
      messages-buffer-max-lines 1000
      create-lockfiles nil
      load-prefer-newer t
      ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(use-package diminish)

;; religion
; 1. utf-8
; 2. spaces not tabs
; 3. no excess whitespace
; 4. files end with newlines
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)

;; decent theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard)
  (set-face-background 'default "#111")
  (set-face-background 'region "#333"))

(use-package hl-line
  :ensure nil
  :config
  (set-face-background 'hl-line "#222"))

(use-package simple-modeline
  :init (simple-modeline-mode))

;; currently emacs-plus@28 via the emacs-plus tap:
; https://github.com/d12frosted/homebrew-emacs-plus
(when (string-equal "darwin" system-type)
  (setq ns-command-modifier      'meta
        ns-alternate-modifier    'super
        ns-function-modifier     'hyper
        ns-use-native-fullscreen nil)
  (setq dired-use-ls-dired nil)

  ; no i do not want to print
  (unbind-key "s-p"))

;; emacs 30, wayland, nix, madness
(when (string-equal "gnu/linux" system-type)
  (setq default-frame-alist '((undecorated . t)))
  (when (and (display-graphic-p) (not (null (x-list-fonts "FiraCode Nerd Font-10"))))
    (set-frame-font "FiraCode Nerd Font-10" nil t)))

;; just buy into the whole vertico et al ecosystem for now
(use-package vertico
  :init (vertico-mode)
  :config
  (require 'vertico-directory)
  :bind
  (("M-V" . #'vertico-multiform-vertical)
   ("M-G" . #'vertico-multiform-grid)
   ("M-F" . #'vertico-multiform-flat)
   ("M-R" . #'vertico-multiform-reverse)
   ("M-U" . #'vertico-multiform-unobtrusive))
  :hook (vertico-multiform-mode))

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
  :init (savehist-mode))

(use-package marginalia
  :init (marginalia-mode))

(use-package orderless
  :config
  (setq orderless-matching-styles
        '(orderless-literal orderless-regexp orderless-flex))
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :commands
  (consult-buffer consult-focus-lines consult-goto-line consult-imenu consult-ripgrep)
  :bind
  (("C-x b" . consult-buffer)
   ("s-g"   . consult-ripgrep)
   ("<leader>cb" . consult-buffer)
   ("<leader>cg" . consult-ripgrep)
   ("<leader>cf" . consult-focus-lines)
   ("<leader>ci" . consult-imenu)
   ("<leader>cl" . consult-line)
   ("<leader>cL" . consult-goto-line)
   ("<leader>cm" . consult-flymake))
  :config
  (setq consult-narrow-key "<"))

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
  :init
  (setq evil-want-keybinding nil
        evil-undo-system 'undo-fu)
  (modify-syntax-entry ?_ "w")
  (evil-mode)
  :config
  (evil-set-leader '(normal visual motion) (kbd "SPC"))
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "wq" 'kill-this-buffer)

  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'inferior-emacs-lisp-mode 'emacs))

(use-package evil-collection ;; https://github.com/emacs-evil/evil-collection
  :after evil
  :config
  (evil-collection-init)
  (diminish 'evil-collection-unimpaired-mode))

(use-package evil-commentary
  :after evil
  :diminish
  :init (evil-commentary-mode))

(use-package evil-mc
  :after evil
  :diminish
  :init (global-evil-mc-mode))

(use-package evil-cleverparens
  :after (evil smartparens)
  :diminish
  :hook
  ((lisp-mode emacs-lisp-mode) . evil-cleverparens-mode)
  :init
  (setq evil-cleverparens-use-additional-movement-keys nil)
  :config
  (require 'evil-cleverparens-text-objects))

(use-package evil-textobj-line)
(use-package evil-textobj-syntax)

;; qol
; blindly install them all:
; (all-the-icons-install-fonts)
(use-package all-the-icons)

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

(use-package company
  :diminish
  :init (global-company-mode)
  :bind
  (:map company-active-map
        ("RET" . nil)
        ("<return>" . nil)
        ("C-RET" . company-complete-selection)
        ("M-RET" . company-complete-selection))
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-show-quick-access 'left)
  (company-tooltip-align-annotations t))

(use-package deadgrep)

(use-package envrc
  :init (envrc-global-mode)
  :diminish)

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C-_" . er/contract-region)))

(use-package magit
  :bind
  (("C-x g" . magit-status))
  :config
  (setq magit-save-repository-buffers 'dontask))

(use-package popwin
  :init (popwin-mode))

(use-package recentf
  :init (recentf-mode)
  :config
  (setq recentf-max-saved-items 250)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package smartparens
  :diminish
  :config (require 'smartparens-config)
  :hook ((emacs-lisp-mode lisp-mode) . smartparens-mode))

(use-package uniquify
  :ensure nil
  :config (setq uniquify-buffer-name-style 'forward))

(use-package which-key
  :init (which-key-mode)
  :diminish
  :config (setq which-key-idle-delay 0.8))

(use-package windmove
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
                  column-number-mode
                  hl-line-mode
                  show-paren-mode
                  subword-mode))
    (add-hook 'prog-mode-hook mode)
    (setq-local indent-tabs-mode nil)))

(use-package cue-mode)

(use-package dockerfile-mode)

(use-package eldoc
  :ensure nil
  :diminish)

(use-package enh-ruby-mode
  :mode "\\.rb\\'"
  :config
  (setq enh-ruby-preserve-indent-in-heredocs t))

(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (defun pd/setup-go-mode ()
    (setq-local tab-width 4)
    (add-hook 'before-save-hook 'gofmt-before-save nil t))
  (add-hook 'go-mode-hook #'pd/setup-go-mode))

(use-package jsonnet-mode
  :config
  (defun pd/setup-jsonnet-mode ()
    (add-hook 'before-save-hook 'jsonnet-reformat-buffer nil t))
  (add-hook 'jsonnet-mode-hook #'pd/setup-jsonnet-mode))

(use-package lisp-mode
  :ensure nil
  :hook turn-on-eldoc-mode
  :config
  (setq comment-column 0))

; reliably annoying that this doesn't descend from lisp-mode
(use-package emacs-lisp-mode
  :ensure nil
  :hook turn-on-eldoc-mode
  :config
  (setq comment-column 0))

(use-package markdown-mode)

(use-package nix-mode
  :ensure t)
(use-package nixfmt
  :ensure t
  :config
  (add-hook 'nix-mode-hook 'nixfmt-on-save-mode))

(use-package rust-mode
  :config
  (setq rust-format-on-save t))

(use-package rust-ts-mode
  :config
  (setq rust-format-on-save t))

(use-package sh-script
  :ensure nil
  :config
  (setq sh-basic-offset 2))

(use-package sops)

(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode))

(use-package whitespace-cleanup-mode
  :diminish
  :hook (prog-mode markdown-mode)
  :custom
  (whitespace-cleanup-mode-only-if-initially-clean nil))

(use-package yaml-mode)

;; ide
(use-package ibuffer
  :ensure nil)

(use-package ibuffer-vc
  :after ibuffer
  :config
  (setq ibuffer-default-sorting-mode 'filename/process
        ibuffer-show-empty-filter-groups nil)
  (defun pd/prepare-ibuffer ()
    (ibuffer-auto-mode +1)
    (ibuffer-vc-set-filter-groups-by-vc-root))
  (add-hook 'ibuffer-mode-hook 'pd/prepare-ibuffer))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l") ; so it at least doesn't steal s-l
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)
         (nix-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred))
  :config
  (setq read-process-output-max (* 1024 1024))
  ; emulate <leader>l being the lsp-keymap-prefix
  (evil-define-key '(normal visual) 'lsp-mode (kbd "SPC l") lsp-command-map)
  (evil-normalize-keymaps))

(use-package lsp-ui
  :commands lsp-ui-mode)

;; shell
(use-package vterm
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 50000)
  :hook
  (vterm-mode . evil-emacs-state)
  :config
  (setq vterm-buffer-name-string "*vterm %s*")
  (add-to-list 'vterm-eval-cmds
               '("update-default-directory" (lambda (path)
                                              (setq default-directory path)))))

(defun pd/vterm-buffers ()
  (--filter (with-current-buffer it (eq major-mode 'vterm-mode))
            (buffer-list)))

(defvar consult-vterm-buffer-source
  `(:name "vterm"
          :hidden   nil
          :narrow   ?t
          :category buffer
          :state    ,#'consult--buffer-state
          :items    ,(lambda () (mapcar #'buffer-name (pd/vterm-buffers)))))

(defun pd/vterm-or-consult (&optional arg)
  "Use consult to switch to a vterm.
With no prefix arg, or if no vterms exist, create a new one in default-directory."
  (interactive "P")
  (require 'consult)
  (let* ((terms (pd/vterm-buffers))
         (n (length terms)))
    (cond
     ((or arg (eq n 0))
      (vterm arg))
     ((eq n 1)
      (switch-to-buffer (car terms)))
     (t
      (consult--multi '(consult-vterm-buffer-source))))))

;; junkdrawer
(defun pd/reload-buffer ()
  "Kill the current buffer and immediately reload it without moving point."
  (interactive)
  (let ((path (buffer-file-name)) (point (point)))
    (kill-buffer)
    (find-file path)
    (goto-char point)))

(defun pd/find-init.el ()
  "find-file init.el"
  (interactive)
  (find-file (expand-file-name user-init-file)))

(defun pd/comment-dwim (arg)
  "If the region is active (`mark-active') and `transient-mark-mode'
is on, let's `comment-dwim' do its thing.
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


;;; maybe
(use-package treesit
  :ensure nil
  :preface
  (defun pd/treesit-install-grammars ()
    (interactive)
    (dolist (grammar
             '((css "https://github.com/tree-sitter/tree-sitter-css")
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
               (json "https://github.com/tree-sitter/tree-sitter-json")
               (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
               (rust "https://github.com/tree-sitter/tree-sitter-rust")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
               (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  (dolist (remap '((css-mode . css-ts-mode)
                   (js-mode . js-ts-mode)
                   (json-mode . json-ts-mode)
                   (ruby-mode . ruby-ts-mode)
                   (rust-mode . rust-ts-mode)
                   (typescript-mode . tsx-ts-mode)
                   (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist remap))

  :config
  (pd/treesit-install-grammars))

(use-package emacs
  :ensure nil
  :init
  (unbind-key "M-t")

  :bind
  (("<leader>bz" . pd/reload-buffer)
   ("<leader>bb" . ibuffer)

   ;; misc
   ("M-;"     . pd/comment-dwim)
   ("M-'"     . pd/vterm-or-consult)
   ("C-x C-b" . ibuffer)
   ("C-x C-d" . dired)
   ("C-x d"   . dired)
   ("C-c w"   . delete-trailing-whitespace)
   ("C-c \\"  . delete-horizontal-whitespace)
   ("C-c ="   . align-regexp)
   ("C-M-+"   . text-scale-increase)
   ("C-M-_"   . text-scale-decrease)

   ;; jumps
   ("<leader>je" . consult-flymake) ; also cm, but i think of it as a "jump" often
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
   ("<leader>xs" . pd/vterm-or-consult)))
