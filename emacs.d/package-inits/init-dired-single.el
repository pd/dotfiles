(define-key dired-mode-map [return] 'joc-dired-single-buffer)
(define-key dired-mode-map "^"
  (function (lambda () (interactive) (joc-dired-single-buffer ".."))))
