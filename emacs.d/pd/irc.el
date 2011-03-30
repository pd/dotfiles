(autoload 'erc "erc" "Emacs IRC Client" t)

(defun pd/configure-erc ()
  (setq erc-nick "pd"
        erc-nick-uniquifier "_"
        erc-full-name "pd"
        erc-email-userid "philo"
        erc-max-buffer-size 5000
        erc-join-buffer 'window-noselect
        erc-log-channels-directory "~/.erc/logs"
        erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"
                                     "324" "329" "332" "333" "353" "477")
        erc-autojoin-channels-alist pd/erc-secrets-autojoins
        erc-ignore-list pd/erc-secrets-ignores)

  (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
  (add-hook 'erc-mode-hook 'pd/turn-off-show-trailing-whitespace)
  (add-hook 'erc-text-matched-hook 'pd/irc-hilited)
  (add-hook 'erc-server-PRIVMSG-functions 'pd/hilite-on-pm))

(eval-after-load 'erc
  '(progn (pd/configure-erc)))

; simplified version of http://www.emacswiki.org/emacs/ErcPageMe
(defun pd/hilite-on-pm (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg)))
      (pd/irc-hilited "pm" nick msg)
      nil)))

(defun pd/irc-hilited (msg-type who msg)
  (pd/x-urgency-hint (selected-frame) t))

(defun pd/irc ()
  "Connect to IRC, maybe."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (dolist (srv pd/erc-secrets-servers)
      (when (y-or-n-p (concat (cadr srv) "? "))
        (apply 'erc srv)))))

(defalias 'irc 'pd/irc)

(provide 'pd/irc)
