(defun pd/isearch-yank-region ()
  (interactive)
  (when (region-active-p) (deactivate-mark))
  (isearch-yank-internal 'mark))

(define-key isearch-mode-map (kbd "C-o") 'pd/isearch-yank-region)
