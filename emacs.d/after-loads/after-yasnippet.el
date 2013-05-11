(yas-global-mode +1)

(define-key yas-minor-mode-map (kbd "C-M-,") 'yas-expand)
(define-key yas-minor-mode-map (kbd "TAB")   nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)

(add-hook 'snippet-mode-hook (lambda ()
                               (setq mode-require-final-newline nil)))
