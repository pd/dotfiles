(after 'flow-js2-mode
  (add-to-list 'company-backends 'company-flow))

(after 'js
  (setq-default js-indent-level 2))

(after 'js2-mode
  (setq-default js2-basic-offset 2
                js2-bounce-indent-p nil
                js2-missing-semi-one-line-override t
                js2-include-node-externs t
                js2-include-browser-externs nil
                js2-idle-timer-delay 0.5
                js2-skip-preprocessor-directives t) ; aka, ignore #!env node
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook 'flow-js2-mode)
  (add-hook 'js2-mode-hook 'prettier-mode))

(after 'typescript-mode
  (require 'tide)
  (defun pd/setup-tide-mode ()
    (tide-setup)
    (setq flycheck-check-syntax-automatically '(save mode-enabled)))

  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook 'tide-setup)
  (add-hook 'typescript-mode-hook 'tide-hl-identifier-mode)
  (add-hook 'typescript-mode-hook 'eldoc-mode)
  (add-hook 'typescript-mode-hook 'subword-mode)
  (add-hook 'typescript-mode-hook 'prettier-mode))

(provide 'pd/js)
