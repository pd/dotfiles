; programming is better when these are the case
(setq column-number-mode t
      require-final-newline t)

; strictly specify the order of hippie-expand's search
; for autocompletion targets; eg, try a simple symbol
; completion from the line above this one before trying
; to complete an entire list from a buffer i opened last
; January.
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-dabbrev-all-buffers
        try-expand-list
        try-expand-line
        try-expand-list-all-buffers
        try-expand-line-all-buffers))

; coding-hook a la emacs-starter-kit
(defvar pd/coding-hook nil)

(defun pd/turn-on-hl-line-mode ()
  (hl-line-mode t))

(defun pd/turn-off-show-trailing-whitespace ()
  ; sometimes it's just unnecessary
  (setq show-trailing-whitespace nil))

(defun pd/turn-on-show-paren-mode ()
  (show-paren-mode t))

(defun pd/turn-on-paredit-mode ()
  (paredit-mode t))

(defun pd/enable-newline-and-indent (map)
  (define-key map (kbd "<return>") 'newline-and-indent))

(defun pd/restore-paragraph-movement (map)
  (define-key map (kbd "M-p") 'backward-paragraph)
  (define-key map (kbd "M-n") 'forward-paragraph))

(defun pd/turn-on-hl-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|XXX\\|REFACTOR\\)"
          1 font-lock-warning-face t))))

(add-hook 'pd/coding-hook 'pd/turn-on-hl-line-mode)
(add-hook 'pd/coding-hook 'pd/turn-on-hl-watchwords)

(defun pd/run-coding-hook ()
  (run-hooks 'pd/coding-hook))

(provide 'pd/coding)
