(defvar cue-mode-font-lock-keywords)
(setq cue-mode-font-lock-keywords
      `(
        ("\\(//\\)\\(.*\\)$"
         (1 font-lock-comment-delimiter-face t)
         (2 font-lock-comment-face t))
        ("\\([\\{\\}&]\\)" (1 font-lock-keyword-face))
        ("\\(package\\|import\\||\\|*\\|\\[\\|\\]\\|\\<for\\>\\|\\<in\\>\\)"  (1 font-lock-keyword-face))
        ("\\(#?[A-Za-z0-1_\\ \\?]+: \\)"   (1 font-lock-variable-name-face))
        ("\\([A-Za-z0-1\\ ]+:: \\)"  (1 font-lock-constant-face))
        ("\\<\\(string\\|number\\|bool\\|bytes\\)\\>" (1 font-lock-type-face))
        ("\\(null\\)" (1 font-lock-constant-face))
        ("\\(@tag(\\).*\\()\\)"
         (1 font-lock-builtin-face)
         (2 font-lock-builtin-face))
        ))

(defun cue-format-buffer ()
  "Rewrite current buffer in a canonical format using cue fmt."
  (interactive)
  (let ((buf (get-buffer-create "*cue-fmt*")))
    (if (zerop (call-process-region (point-min) (point-max)
                                    "cue" nil buf nil "fmt" "-"))
        (let ((point (point))
              (window-start (window-start)))
          (erase-buffer)
          (insert-buffer-substring buf)
          (goto-char point)
          (set-window-start nil window-start))
      (message "cue fmt: %s" (with-current-buffer buf (buffer-string))))
    (kill-buffer buf)))

(define-minor-mode cue-format-on-save-mode
  "Run cue-format-buffer before saving current buffer."
  :lighter ""
  (if cue-format-on-save-mode
      (add-hook 'before-save-hook #'cue-format-buffer nil t)
    (remove-hook 'before-save-hook #'cue-format-buffer t)))

;;;###autoload
(define-derived-mode cue-mode prog-mode
  "cue"
  "Major mode for cuelang."
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults) '(cue-mode-font-lock-keywords))
  (setq mode-name "Cue"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cue\\'" . cue-mode))

(provide 'cue-mode)
