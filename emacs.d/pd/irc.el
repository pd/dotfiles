(autoload 'erc "erc" "Emacs IRC Client" t)
(load "~/.erc-secrets.el" 'noerror 'nomessage) ; passwords, autojoin lists, etc

(eval-after-load 'erc
  '(progn
     (setq erc-nick "pd"
           erc-nick-uniquifier "_"
           erc-full-name "pd"
           erc-max-buffer-size 5000
           erc-join-buffer 'window-noselect)
     (setq erc-log-channels-directory "~/.erc/logs")
     (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

     ; only notify about activity for actual conversation
     (setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))
     (setq erc-autojoin-channels-alist pd/erc-secrets-autojoin-alist)))

(defun pd/irc ()
  "Connect to IRC, maybe."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (dolist (server pd/erc-secrets-server-list)
      (when (y-or-n-p (concat server "? "))
        (erc :server server :password pd/erc-secrets-password)))))

(defalias 'irc 'pd/irc)

(provide 'pd/irc)
