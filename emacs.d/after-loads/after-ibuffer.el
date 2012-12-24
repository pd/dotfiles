; ibuffer grouping!
(setq ibuffer-saved-filter-groups
      '(("pd"
         ("zabxuq" (filename . "zabxuq"))
         ("bzork" (filename . "bzork"))
         ("sauce" (filename . "sauce"))
         ("el-get/recipes" (filename . "el-get/recipes"))
         ("el-get" (filename . "el-get"))
         ("emacs.d" (filename . "emacs.d"))
         ("dotfiles" (filename . "dotfiles"))
         ("gems" (filename . ".rvm/gems"))
         ("terms" (or (mode . term-mode)
                      (mode . shell-mode)))
         ("magit" (name . "\*magit"))
         ("erc" (mode . erc-mode))
         ("system" (or (name . "\*Help\*")
                       (name . "\*Apropos\*")
                       (name . "\*info\*")
                       (name . "\*Backtrace\*")
                       (name . "\*Completions\*")
                       (name . "\*Messages\*")
                       (name . "\*scratch\*"))))))

(setq ibuffer-show-empty-filter-groups nil)
(defvar pd/default-ibuffer-filter-group "pd")
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups pd/default-ibuffer-filter-group)))
