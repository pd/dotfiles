; how to open urls
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (cond ((pd/macosx-p) "open")
                                       ((executable-find "chromium-dev") "chromium-dev")
                                       ((executable-find "chromium") "chromium")
                                       (t "firefox")))

; why does woman open its own frame by default?
(setq woman-use-own-frame nil)

; ibuffer grouping!
(setq ibuffer-saved-filter-groups
      '(("pd"
         ("zabxuq" (filename . "zabxuq"))
         ("bzork" (filename . "bzork"))
         ("sauce" (filename . "sauce"))
         ("emacs.d" (filename . "emacs.d"))
         ("dotfiles" (filename . "dotfiles"))
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

; run-ruby etc should put the buffer in my current window,
; not a seemingly random different one ...
(nconc same-window-buffer-names '("*ruby*" "*js*"))

(provide 'pd/misc)
