(autoload 'ioke-mode "ioke-mode" "Major mode for ioke" t)
(autoload 'run-ioke "inf-ioke" "Inferior mode for ioke" t)

(add-to-list 'auto-mode-alist '("\\.ik\\'" . ioke-mode))
