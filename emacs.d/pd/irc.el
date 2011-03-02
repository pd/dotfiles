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
     (setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"
                                     "324" "329" "332" "333" "353" "477"))
     (setq erc-autojoin-channels-alist pd/erc-secrets-autojoins)))

(defun pd/irc ()
  "Connect to IRC, maybe."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (turn-off-save-visited-files-mode)
    (dolist (srv pd/erc-secrets-servers)
      (when (y-or-n-p (concat (cadr srv) "? "))
        (apply 'erc srv)))))

(defalias 'irc 'pd/irc)

(provide 'pd/irc)
