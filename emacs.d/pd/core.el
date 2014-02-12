(electric-indent-mode +1)
(winner-mode +1)
(ido-mode +1)
(quickref-global-mode +1)
(recentf-mode +1)
(toggle-save-place-globally)
(setq auto-save-default nil)

(after 'auto-complete
  (require 'auto-complete-config)
  (require 'fuzzy)

  (ac-config-default)
  (setq ac-auto-start 3
        ac-auto-show-menu 0.5
        ac-comphist-file "~/.emacs.d/.crap/ac-comphist.dat")

  (bind-key "C-n" 'ac-next ac-complete-mode-map)
  (bind-key "C-p" 'ac-previous ac-complete-mode-map)
  (bind-key "C-l" 'ac-expand-common ac-complete-mode-map))

(after 'bookmark
  (setq bookmark-default-file (expand-file-name ".crap/bookmarks" user-emacs-directory)))

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
  (setq ido-save-directory-list-file "~/.emacs.d/.crap/ido.last"
        ido-enable-flex-matching t
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-use-virtual-buffers t))

(after 'multiple-cursors
  (setq mc/list-file (expand-file-name "store/mc-lists.el" user-emacs-directory)))

(after 'package
  (require 'package-build)
  (pd/load-ext 'package)
  (require 'melpa-upstream-visit)
  (setq muv:button-location 'package-name))

(after 'quickref
  (quickref-global-mode +1)
  (setq quickref-save-file (expand-file-name "store/quickrefs.el" user-emacs-directory)))

(after 'recentf
  (setq recentf-save-file (expand-file-name ".crap/recentf" user-emacs-directory)
        recentf-max-menu-items 10))

(after 'saveplace
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/.crap/saveplace.dat"))

(after 'smex
  (smex-initialize)
  (setq smex-save-file "~/.emacs.d/.crap/smex.save")
  (smex-auto-update 120))

(after 'uniquify
  (setq uniquify-buffer-name-style 'forward))

(after 'yasnippet
  (bind-key   "C-M-," 'yas-expand yas-minor-mode-map)
  (unbind-key "TAB"   yas-minor-mode-map)
  (unbind-key "<tab>" yas-minor-mode-map))

(provide 'pd/core)
