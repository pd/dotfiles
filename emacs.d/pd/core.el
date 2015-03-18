(electric-indent-mode +1)
(winner-mode +1)
(ido-mode +1)
(global-discover-mode +1)
(quickref-global-mode +1)
(recentf-mode +1)
(toggle-save-place-globally)
(setq auto-save-default nil)

;; eval-expression (M-:) shows eldoc in the modeline
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; (after 'auto-complete
;;   (require 'auto-complete-config)
;;   (require 'fuzzy)

;;   (ac-config-default)
;;   (setq ac-auto-start 3
;;         ac-auto-show-menu 0.5
;;         ac-comphist-file (locate-user-emacs-file ".crap/ac-comphist.dat"))

;;   (bind-key "C-n" 'ac-next ac-complete-mode-map)
;;   (bind-key "C-p" 'ac-previous ac-complete-mode-map)
;;   (bind-key "C-l" 'ac-expand-common ac-complete-mode-map))

(after 'bookmark
  (setq bookmark-default-file (locate-user-emacs-file ".crap/bookmarks")))

(after 'company
  (add-hook 'after-init-hook 'global-company-mode))

(after 'dired
  (require 'dired-details+)
  (setq-default dired-details-hidden-string "--- ")
  (dired-details-install))

(after 'ffap
  (pd/load-ext 'ffap))

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

(after 'multiple-cursors
  (setq mc/list-file (locate-user-emacs-file "store/mc-lists.el")))

(after 'package
  (require 'package-build)
  (require 'paradox)
  (pd/load-ext 'package))

(after 'paradox
  ; Stop asking me if I want to star/unstar shit on github ffs.
  (setq paradox-github-token t))

(after 'projectile
  (setq projectile-known-projects-file (locate-user-emacs-file "store/projectile-bookmarks.eld")))

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

(provide 'pd/core)
