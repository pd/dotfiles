(autoload 'erc "erc" "Emacs IRC Client" t)

(eval-after-load 'erc
  '(progn
     (setq erc-nick "pd"
           erc-nick-uniquifier "_"
           erc-full-name "Kyle Hargraves"
           erc-max-buffer-size 20000)

     (setq erc-log-channels-directory "~/.erc/logs")
     (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

     ; only show activity for actual conversation, not joins/parts/&c
     (setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))

     ; Passwords and autojoin lists are loaded from a file ignored by git.
     (require 'erc-super-secret)
     (setq erc-autojoin-channels-alist erc-super-secret-autojoin-alist)))

(setq default-erc-server-list '("irc.freenode.net"))
(defun irc-please ()
  "Connect to IRC, maybe. And prompt for each server to ensure we want to connect to it."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (dolist (server default-erc-server-list)
      (when (y-or-n-p (concat server "? "))
        (erc :server server :password erc-super-secret-password)))))

(defalias 'irc 'irc-please)
