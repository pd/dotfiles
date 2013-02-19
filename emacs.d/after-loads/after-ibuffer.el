; ibuffer grouping!
(setq ibuffer-default-sorting-mode 'filename/process
      ibuffer-show-empty-filter-groups nil)

(setq ibuffer-saved-filter-groups
      '(("pd"
         ("emacs.d" (filename . "emacs.d"))
         ("dotfiles" (filename . "dotfiles"))
         ("sauce" (filename . "sauce"))
         ("terms" (or (mode . term-mode)
                      (mode . shell-mode)))
         ("magit" (name . "\*magit"))
         ("gems" (or (filename . ".rvm/gems")
                     (filename . "/.rbenv/")))
         ("system" (or (name . "\*Help\*")
                       (name . "\*Apropos\*")
                       (name . "\*info\*")
                       (name . "\*Backtrace\*")
                       (name . "\*Completions\*")
                       (name . "\*Messages\*")
                       (name . "\*scratch\*"))))))

(defvar pd/default-ibuffer-filter-group "pd")

(defun pd/prepare-ibuffer ()
  (ibuffer-auto-mode 1)
  (ibuffer-switch-to-saved-filter-groups pd/default-ibuffer-filter-group))

(add-hook 'ibuffer-mode-hook 'pd/prepare-ibuffer)
