(let ((erc-secrets (f-expand "erc-secrets.el" user-private-emacs-directory)))
  (when (f-exists? erc-secrets)
    (load erc-secrets)
    (setq znc-servers pd-secrets/znc-servers)))

(after 'erc
  (setq erc-hide-list '("JOIN" "PART" "QUIT")))

(defun pd/erc-buffer-list ()
  (mapcar 'buffer-name (erc-buffer-list)))

(defun pd/helm-erc-switch-buffer ()
  "Switch ERC buffers via Helm."
  (interactive)
  (helm :sources '((name . "ERC Channels")
                   (candidates . pd/erc-buffer-list)
                   (action . helm-switch-to-buffers))
        :buffer "*helm-erc-channels*"))

(defun pd/znc-network-slugs ()
  "Return the list of all network slugs defined in `znc-servers'."
  (loop for (host port ssl users) in znc-servers
        appending (loop for (slug user pass) in users
                        collecting slug)))

(defun pd/helm-znc-connect-to-network ()
  "Connect to a defined ZNC network with ERC."
  (interactive)
  (helm :sources '((name . "ZNC Networks")
                   (candidates . pd/znc-network-slugs)
                   (action . znc-erc))
        :buffer "*helm-znc-networks*"))

(require 'bind-key)
(bind-key "C-x i" 'pd/helm-erc-switch-buffer) ; I never use ido-insert-file
(bind-key "C-x C-i" 'pd/helm-znc-connect-to-network)

(provide 'pd/irc)
