(after 'comint
  (pd/load-ext 'comint)
  (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
  (bind-key "C-c M-o" 'pd/comint-truncate-buffer comint-mode-map))

(after 'shell-switcher
  (shell-switcher-mode)
  (setq shell-switcher-new-shell-function 'shell-switcher-make-shell)
  (add-hook 'shell-mode-hook 'shell-switcher-manually-register-shell))

(defun pd/shell-directory-tracking ()
  "dirtrack > shell-dirtrack for serious"
  (shell-dirtrack-mode -1)
  (dirtrack-mode +1)
  (setq dirtrack-list '("\\`\\(direnv: .+[\r\n]*\\)*[\r\n ]*\\([^\n ]+\\) .*» \\'" 2)))

(after 'shell
  (setq shell-prompt-pattern "^[^\n]*[#$%>»] *")
  (bind-key "C-c d" 'dirs shell-mode-map)
  (bind-key "C-c C-l" 'helm-comint-input-ring shell-mode-map)

  (add-hook 'shell-mode-hook 'pcomplete-shell-setup)
  (add-hook 'shell-mode-hook 'pd/comint-disable-echo)
  (add-hook 'shell-mode-hook 'pd/shell-directory-tracking)

  ; After the shell is running, pretend the directory changed for the first time.
  (add-hook 'shell-mode-hook 'pd/dirtrack-directory-changed))

(after 'pcomplete
  (defalias 'pcomplete/g 'pcomplete/git))

(after 'pcmpl-git
  (push "delete-merged-branches" pcmpl-git-commands))

(after 'dirtrack
  (add-hook 'dirtrack-directory-change-hook 'pd/dirtrack-directory-changed))

;; Actually editing .sh files.
(after 'sh-script
  (setq-default sh-basic-offset 2
                sh-indentation 2))

;; Support expanding my zsh `hash -d` directory aliases
(defvar pd/zsh-directory-aliases nil)
(defvar pd/zsh-directory-aliases-loaded nil)

(defun pd/load-zsh-directory-aliases ()
  (when (not pd/zsh-directory-aliases-loaded)
    (setq pd/zsh-directory-aliases-loaded t)
    (with-temp-buffer
      (apply 'call-process "/usr/bin/env" nil t nil '("zsh" "-i" "-c" "hash -d"))
      (let* ((output (buffer-substring-no-properties (point-min) (point-max)))
             (lines  (--remove (s-blank? it) (s-lines output)))
             (pairs  (--map (s-split-up-to "=" it 1) lines)))
        (setq pd/zsh-directory-aliases pairs)))))

(defun pd/expand-zsh-aliases (filename)
  (pd/load-zsh-directory-aliases)
  (--reduce-from (let ((prefix (concat "~" (car it))))
                   (if (s-starts-with? prefix acc)
                       (s-replace prefix (cadr it) acc)
                     acc))
                 filename
                 pd/zsh-directory-aliases))

(defun pd/expand-file-name--zsh-aliases (orig-fun &rest args)
  (let ((expanded (pd/expand-zsh-aliases (car args))))
    (apply orig-fun expanded (cdr args))))

(defun pd/shell-prefixed-directory-name--zsh-aliases (orig-fun dirname)
  (apply orig-fun (list (pd/expand-zsh-aliases dirname))))

(defun pd/file-name-absolute-p--zsh-aliases (orig-fun fname)
  (apply orig-fun (list (pd/expand-zsh-aliases fname))))

(advice-add 'shell-prefixed-directory-name :around #'pd/shell-prefixed-directory-name--zsh-aliases)
(advice-add 'expand-file-name :around #'pd/expand-file-name--zsh-aliases)
(advice-add 'file-name-absolute-p :around #'pd/file-name-absolute-p--zsh-aliases)

(provide 'pd/shell)
