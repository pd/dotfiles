; lsp
; hcl?
; https://github.com/cyrus-and/zoom

;; ref:
;; https://github.com/milkypostman/dotemacs/blob/main/init.el
;; https://github.com/jjuliano/sensible.emacs.d/blob/main/config/01-packages.el

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
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

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
      load-prefer-newer t)
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
  (load-theme 'gruvbox-dark-hard))

(use-package simple-modeline
  :init (simple-modeline-mode))

;; currently emacs 29 via the emacs-plus tap:
;; https://github.com/d12frosted/homebrew-emacs-plus
(when (string-equal "darwin" system-type)
  (setq ns-command-modifier      'meta
        ns-alternate-modifier    'super
        ns-function-modifier     'hyper
        ns-use-native-fullscreen nil)
  ; no i do not want to print
  (unbind-key "s-p"))

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
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :commands (consult-buffer consult-goto-line consult-ripgrep)
  :bind (("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("s-g"   . consult-ripgrep)))

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
  (("<escape>" . keyboard-escape-quit)
   :map evil-insert-state-map
   ("C-)" . sp-forward-slurp-sexp)
   ("C-(" . sp-backward-slurp-sexp))
  :init
  (evil-mode)
  (setq evil-want-keybinding nil
        evil-undo-system 'undo-fu)
  :config
  (evil-set-leader nil (kbd "SPC"))

  (evil-define-key*))

(use-package evil-collection ;; https://github.com/emacs-evil/evil-collection
  :after evil
  :config
  (evil-collection-init)
  (diminish 'evil-collection-unimpaired-mode))

(use-package evil-mc
  :after evil
  :diminish
  :init (global-evil-mc-mode))

(use-package evil-smartparens
  :after (evil smartparens-mode))

;; qol
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

(use-package envrc
  :init (envrc-global-mode)
  :diminish)

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-_" . er/contract-region)))

(use-package magit
  :bind (("C-x g" . magit-status))
  :config (setq magit-save-repository-buffers 'dontask))

(use-package recentf
  :init (recentf-mode)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package popwin
  :init (popwin-mode))

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
(use-package emacs
  :ensure nil
  :config
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'column-number-mode)
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (add-hook 'prog-mode-hook 'show-paren-mode)
  (add-hook 'prog-mode-hook 'subword-mode))

(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (defun pd/setup-go-mode ()
    (add-hook 'before-save-hook 'gofmt-before-save nil t))
  (add-hook 'go-mode-hook #'pd/setup-go-mode))

(use-package jsonnet-mode
  :config
  (defun pd/setup-jsonnet-mode ()
    (add-hook 'before-save-hook 'jsonnet-reformat-buffer nil t))
  (add-hook 'jsonnet-mode-hook #'pd/setup-jsonnet-mode))

(use-package eldoc
  :ensure nil
  :diminish)

(use-package lisp-mode
  :ensure nil
  :config
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'turn-on-eldoc-mode))

(use-package terraform-mode)

;; ide
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)))

(use-package lsp-ui
  :commands lsp-ui-mode)
(use-package tree-sitter)
(use-package tree-sitter-langs)

;; shell
; https://github.com/CeleritasCelery/emacs-native-shell-complete
(use-package shell
  :ensure nil
  :config
  (defun pd/setup-shell-mode ()
    (shell-dirtrack-mode -1)
    (dirtrack-mode +1))
  
  (add-hook 'shell-mode-hook #'pd/setup-shell-mode))

(use-package dirtrack
  :after shell
  :ensure nil
  :config
  (setq dirtrack-list '("\\`\\(direnv: .+[\r\n]*\\)*[\r\n ]*\\([^\n ]+\\) .*» \\'" 2)))

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

(use-package emacs
  :init
  (unbind-key "M-t")
  :bind
  (("C-c b z"  . pd/reload-buffer)
   ("C-c d"    . dired)
   ("C-c w"    . delete-trailing-whitespace)
   ("C-c \\"   . delete-horizontal-whitespace)
   ("C-c ="    . align-regexp)

   ;; jumps
   ("C-c j f" . find-function)
   ("C-c j i" . pd/find-init.el)
   ("C-c j l" . find-library)
   ("C-c j k" . find-function-on-key)
   ("C-c j v" . find-variable)

   ;; transpositions
   ("M-t c" . transpose-chars)
   ("M-t w" . transpose-words)
   ("M-t l" . transpose-lines)
   ("M-t s" . transpose-sexps)))

;;; wip
(use-package all-the-icons)
