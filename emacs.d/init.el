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
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file)
  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; i've got plenty of ram
(setq gc-cons-threshold (* 128 1024 1024))

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

;; absorb the crazy nix PATH shenanigans
(defun pd/mise-bin-paths ()
  (when (executable-find "mise")
    (with-temp-buffer
      (call-process "mise" nil t nil "bin-paths" "-q")
      (split-string (buffer-substring (point-min) (point-max)) "\n"))))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-arguments nil)
  :config
  (when (or (daemonp) (memq window-system '(ns pgtk)))
    (exec-path-from-shell-initialize)

    ; somehow this isn't functioning when running through
    ; exec-path-from-shell, so just do it by hand for now
    (dolist (path (pd/mise-bin-paths))
      (add-to-list 'exec-path path))))

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
  :config
  (simple-modeline-mode))

;; currently emacs-plus@30 via the emacs-plus tap:
; https://github.com/d12frosted/homebrew-emacs-plus
(when (string-equal "darwin" system-type)
  (setq ns-command-modifier      'meta
        ns-alternate-modifier    'super
        ns-function-modifier     'hyper
        ns-use-native-fullscreen nil)
  (setq dired-use-ls-dired nil)
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . nil))

  ; no i do not want to print
  (unbind-key "s-p"))

;; emacs 30, wayland, nix, madness
(when (string-equal "gnu/linux" system-type)
  (add-to-list 'default-frame-alist '(undecorated . t))
  (add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font-10")))

;; just buy into the whole vertico et al ecosystem for now
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
  (("C-x b" . consult-buffer)
   ("<leader>cb" . consult-buffer)
   ("<leader>cg" . consult-ripgrep)
   ("<leader>ce" . consult-flymake)
   ("<leader>ci" . consult-imenu)
   ("<leader>cl" . consult-line)
   ("<leader>cL" . consult-goto-line)
   ("<leader>cm" . consult-focus-lines)) ; "matching"
  :config
  (setq consult-narrow-key "<"))

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

   :map embark-file-map
   ("$" . pd/vterm-at))
  :config
  ;; enable if i never develop the muscle memory:
  ;; (setq embark-prompter 'embark-completing-read-prompter)
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

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

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
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Causing Help to fail to display at all, eg C-h f whatever:
;;   popwin:close-popup-window-timer: error: (error Attempt to delete main window of frame #<frame *Warnings* 0x7f984648da68>)
;;   delete-other-windows: Cannot make side window the only window [2 times]
;;
;; (use-package popper
;;   :init
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*"
;;           "\\*Backtrace\\*"
;;           help-mode
;;           compilation-mode
;;           ("\\*Warnings\\*" . hide)))
;;   (popper-mode +1)
;;   (popper-echo-mode +1)
;;   :bind
;;   (("s-`" . popper-toggle)
;;    ("M-`" . popper-cycle)))

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
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

(use-package deadgrep
  :bind
  ("s-g" . deadgrep)
  :config
  (defun pd/deadgrep-project-root ()
    "deadgrep current dir in dired-mode"
    (if (eq major-mode 'dired-mode) default-directory
      (deadgrep--project-root)))
  (setq deadgrep-project-root-function 'pd/deadgrep-project-root))

(use-package envrc
  :init (envrc-global-mode)
  :diminish)

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C-_" . er/contract-region)))

(use-package git-link
  :custom
  (get-link-use-commit t))

(use-package magit
  :bind
  (("C-x g" . magit-status))
  :config
  (setq magit-save-repository-buffers 'dontask))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-completion
  :after nerd-icons
  :config (nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :after nerd-icons
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package popwin
  :init (popwin-mode))

(use-package recentf
  :ensure nil
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
  :ensure nil
  :init (which-key-mode)
  :diminish
  :config (setq which-key-idle-delay 0.8))

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

(use-package inf-ruby)

(use-package go-ts-mode
  :custom
  (go-ts-mode-indent-offset 4)
  :config
  (reformatter-define pd/gofmt :program "goimports")
  (add-hook 'go-ts-mode-hook (lambda ()
                               (pd/gofmt-on-save-mode +1)
                               (setq tab-width 4)))

  ; https://github.com/golang/tools/blob/8d38122b0b1a9991f490aa06b7bfca7b4140bdad/gopls/doc/emacs.md#configuring-eglot
  ; so eglot starts LSP in a reasonable spot when jumping into ~/go/pkg/... et al
  (require 'project)

  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (add-hook 'project-find-functions #'project-find-go-module))

(use-package jsonnet-mode
  :config
  (reformatter-define pd/jsonnetfmt :program "jsonnetfmt" :args '("-"))
  (add-hook 'jsonnet-mode-hook #'pd/jsonnetfmt-on-save-mode))

(use-package just-ts-mode
  :config
  (add-hook 'just-ts-mode-hook (lambda ()
                                 (setq tab-width 4))))

(use-package lisp-data-mode
  :ensure nil
  :hook turn-on-eldoc-mode
  :config
  (setq comment-column 0))

(use-package markdown-mode)

(use-package nix-mode
  :config
  (defun pd/nix-repl-from-project-root (orig &rest args)
    (let ((root (project-root (project-current))))
      (let ((default-directory (or root default-directory)))
        (apply orig args))))
  (advice-add 'nix-repl :around #'pd/nix-repl-from-project-root))

(use-package nixfmt
  :config
  (add-hook 'nix-mode-hook 'nixfmt-on-save-mode))

(use-package rust-mode
  :config
  (setq rust-format-on-save t))

(use-package sh-script
  :ensure nil
  :config
  (setq sh-basic-offset 2))

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
  :init
  (setq terraform-format-on-save nil)

  :config
  (setq pd/tffmt
        (or (executable-find "tofu") (executable-find "terraform")))
  (reformatter-define pd/tffmt
    :program pd/tffmt
    :args '("fmt" "-no-color" "-"))
  (add-hook 'terraform-mode-hook 'pd/tffmt-on-save-mode))

(use-package wgrep)

(use-package whitespace-cleanup-mode
  :diminish
  :hook (prog-mode markdown-mode)
  :custom
  (whitespace-cleanup-mode-only-if-initially-clean nil))

(use-package yaml-mode)

(use-package zig-ts-mode)

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
  ; complete only with TAB, not RET
  (keymap-unset corfu-map "RET"))

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

(use-package eglot
  :hook
  ((go-mode go-ts-mode nix-mode zig-ts-mode) . eglot-ensure)
  :bind
  (("<leader>la" . eglot-code-actions)
   ("<leader>lf" . eglot-format-buffer)
   ("<leader>lr" . eglot-rename)
   ("<leader>lx" . eglot-shutdown)
   ("<leader>lX" . eglot-shutdown-all)
   ("<leader>lz" . eglot-reconnect))
  :config
  (add-to-list 'eglot-server-programs '(zig-ts-mode . ("zls"))))

(use-package reformatter)

(use-package tramp
  :ensure nil
  :autoload (tramp-parse-sconfig))

(use-package treesit
  :ensure nil
  :preface
  (defun pd/treesit-install-grammars ()
    (interactive)
    (dolist (grammar
             '((css "https://github.com/tree-sitter/tree-sitter-css")
               (go "https://github.com/tree-sitter/tree-sitter-go")
               (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
               (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
               (just "https://github.com/IndianBoy42/tree-sitter-just")
               (json "https://github.com/tree-sitter/tree-sitter-json")
               (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
               (rust "https://github.com/tree-sitter/tree-sitter-rust")
               (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
               (zig "https://github.com/maxxnino/tree-sitter-zig")))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  (dolist (remap '((css-mode        . css-ts-mode)
                   (go-mode         . go-ts-mode)
                   (just-mode       . just-ts-mode)
                   (js-mode         . js-ts-mode)
                   (json-mode       . json-ts-mode)
                   (ruby-mode       . ruby-ts-mode)
                   (rust-mode       . rust-ts-mode)
                   (typescript-mode . tsx-ts-mode)
                   (zig-mode        . zig-ts-mode)))
    (add-to-list 'major-mode-remap-alist remap))

  :config
  (pd/treesit-install-grammars))

;; shell
(use-package vterm
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 50000)
  (vterm-clear-scrollback-when-clearing t)
  :hook
  (vterm-mode . evil-emacs-state)
  :bind
  (:map vterm-mode-map                  ; reclaim some bindings
   ("M-'"   . pd/vterm-or-consult)
   ("M-\""  . vterm)
   ("M-s-'" . pd/vterm-on))
  :config
  (setq vterm-buffer-name-string "*vterm %s*"
        vterm-tramp-shells '(("ssh" "zsh"))
        vterm-copy-mode-remove-fake-newlines t)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'vterm-eval-cmds
               '("update-default-directory" (lambda (path)
                                              (setq default-directory path))))
  (defun pd/vterm-term-prompt-regexp ()
    (setq term-prompt-regexp "^> "))
  (add-hook 'vterm-mode-hook 'pd/vterm-term-prompt-regexp))

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

(defun pd/vterm-at (path)
  (interactive "fDir: \n")
  (let ((default-directory (if (file-directory-p path) path
                             (file-name-directory path))))
    (vterm)))

(defun pd/vterm-on (host)
  "vterm on a host"
  (interactive
   (let* ((sshconfig (expand-file-name "~/.ssh/config"))
          (hosts (remq nil (mapcar 'cadr (tramp-parse-sconfig sshconfig)))))
     (list (completing-read "Host: " (nconc '("localhost") hosts)))))
  (if (string-equal host "localhost")
      (pd/vterm-at "~")
    (pd/vterm-at (format "/ssh:%s:." host))))

(defun pd/vterm-or-consult (&optional arg)
  "Use consult to switch to a vterm.
With prefix arg, or if no vterms exist, create a new one in default-directory."
  (interactive "P")
  (require 'consult)
  (let* ((terms (pd/vterm-buffers))
         (n (length terms)))
    (cond
     ; no vterms, explicit prefix arg, or single vterm that is our current buffer
     ((or arg (eq n 0) (and (eq n 1) (eq major-mode 'vterm-mode)))
      (vterm arg))
     ; one vterm that isn't our current buffer
     ((and (eq n 1) (not (eq major-mode 'vterm-mode)))
      (switch-to-buffer (car terms)))
     (t
      (consult--multi '(consult-vterm-buffer-source))))))

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
   ("M-'"     . pd/vterm-or-consult)
   ("M-\""    . vterm)
   ("M-s-'"   . pd/vterm-on)
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
