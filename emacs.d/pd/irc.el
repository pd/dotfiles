(let ((erc-secrets (f-expand "erc-secrets.el" user-private-emacs-directory)))
  (when (f-exists? erc-secrets)
    (load erc-secrets)
    (setq znc-servers pd-secrets/znc-servers)))

(after 'erc
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))

  (defun pd/erc-buffer-list ()
    (mapcar 'buffer-name (erc-buffer-list)))

  (defun pd/helm-erc-switch-buffer ()
    "Switch ERC buffers via Helm"
    (interactive)
    (helm :sources '((name . "ERC Channels")
                     (candidates . pd/erc-buffer-list)
                     (action . helm-switch-to-buffers))
          :buffer "*helm-erc-channels*")))

(require 'bind-key)
(bind-key "C-x i" 'pd/helm-erc-switch-buffer) ; I never use ido-insert-file

(provide 'pd/irc)
