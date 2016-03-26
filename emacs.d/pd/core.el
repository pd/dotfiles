(setq auto-save-default nil
      save-interprogram-paste-before-kill t)

(column-number-mode +1)
(electric-indent-mode +1)
(winner-mode +1)
(ido-mode +1)
(quickref-global-mode +1)
(recentf-mode +1)
(toggle-save-place-globally)
(which-key-mode +1)
(global-linum-mode +1)

(require 'popwin)
(popwin-mode +1)

;; eval-expression (M-:) shows eldoc in the modeline
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

(after 'bookmark
  (setq bookmark-default-file (locate-user-emacs-file ".crap/bookmarks")))

(after 'company
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-tooltip-align-annotations t)

  (bind-keys :map company-mode-map
             ("C-<tab>" . company-manual-begin))

  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)))

(after 'dired
  (require 'dired-details+)
  (setq-default dired-details-hidden-string "--- ")
  (dired-details-install))

(after 'helm
  (require 'helm-projectile)
  (setq helm-M-x-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-imenu-fuzzy-match t))

(after 'helm-mt
  (setq helm-mt/all-terminal-modes '(shell-mode term-mode)))

(after 'ffap
  (pd/load-ext 'ffap))

(after 'flycheck
  (global-flycheck-mode +1)
  (when (require 'flycheck-cask nil 'noerror)
    (add-hook 'flycheck-mode-hook #'flycheck-cask-setup)))

(after 'ibuffer
  (require 'ibuffer-vc)
  (setq ibuffer-default-sorting-mode 'filename/process
        ibuffer-show-empty-filter-groups nil
        ibuffer-formats '((mark modified read-only vc-status-mini " "
                                (name 18 18 :left :elide)
                                " "
                                (size 9 -1 :right)
                                " "
                                (mode 16 16 :left :elide)
                                " "
                                (vc-status 16 16 :left)
                                " "
                                filename-and-process)))

  (defun pd/prepare-ibuffer ()
    (ibuffer-auto-mode 1)
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))

  (add-hook 'ibuffer-mode-hook 'pd/prepare-ibuffer))

(after 'ido
  (ido-everywhere t)
  (setq ido-save-directory-list-file (locate-user-emacs-file ".crap/ido.last")
        ido-enable-flex-matching t
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-use-virtual-buffers t)

  (require 'flx-ido)
  (flx-ido-mode +1))

(after 'linum
  (require 'linum-off)
  (add-to-list 'linum-disabled-modes-list 'erc-mode))

(after 'package
  (require 'package-build)
  (require 'paradox))

(after 'paradox
  ; Stop asking me if I want to star/unstar shit on github ffs.
  (setq paradox-github-token t
        paradox-execute-asynchronously t))

(after 'projectile
  (setq projectile-known-projects-file (locate-user-emacs-file "store/projectile-bookmarks.eld")
        projectile-cache-file (locate-user-emacs-file "store/projectile.cache")
        projectile-switch-project-action 'helm-projectile
        projectile-enable-caching t)
  ; Force it to reload after having change the value
  (projectile-load-known-projects))

(after 'quickref
  (quickref-global-mode +1)
  (setq quickref-save-file (locate-user-emacs-file "store/quickrefs.el")))

(after 'recentf
  (setq recentf-save-file (locate-user-emacs-file ".crap/recentf")
        recentf-max-menu-items 10))

(after 're-builder
  (setq reb-re-syntax 'string))

(after 'saveplace
  (setq-default save-place t)
  (setq save-place-file (locate-user-emacs-file ".crap/saveplace.dat")))

(after 'smex
  (smex-initialize)
  (setq smex-save-file (locate-user-emacs-file  ".crap/smex.save"))
  (smex-auto-update 120))

(after 'uniquify
  (setq uniquify-buffer-name-style 'forward))

(after 'which-key
  (setq which-key-idle-delay 0.8))

(provide 'pd/core)
