(setq quickref-plist
      '(paredit-mode
        (("M-r" . "paredit-raise-sexp")
         ("M-s" . "paredit-slice-sexp"))
        ruby-mode
        (("C-c C-z" . "ruby-switch-to-inf")
         ("C-x C-r" . "ruby-send-region")
         ("C-x C-e" . "ruby-send-last-sexp")
         ("C-c C-s" . "inf-ruby"))
        help-mode
        (("C-c C-b" . "help-go-back")
         ("C-c C-f" . "help-go-forward"))))

(quickref-global-mode +1)
; (diminish 'quickref-mode)

