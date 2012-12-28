(require 'ido-other-window)

(defun pd/ido-move-to-home ()
  "Jump to ~ in ido file/dir prompt."
  (interactive)
  (ido-set-current-home)
  (ido-reread-directory))

(defun pd/ido-file-prompt-abbreviate-file-name ()
  "Use `pd/abbreviate-file-name' to alter ido's prompt."
  (setq dirname (pd/abbreviate-file-name dirname)))

(ido-mode t)
(ido-everywhere t)

(setq ido-enable-flex-matching t
      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window
      ido-use-virtual-buffers t
      ido-rewrite-file-prompt-functions '(pd/ido-file-prompt-abbreviate-file-name))

; Tricky to use keydef for this one =\
(define-key ido-file-dir-completion-map (kbd "~") 'pd/ido-move-to-home)
