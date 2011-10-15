(require 'magit) ; force load, autoload is fucked TODO
(global-set-key (kbd "C-M-g") 'magit-status)

(setq magit-save-some-buffers nil
        magit-completing-read 'ido-completing-read
        magit-omit-untracked-dir-contents t) ; show unstaged dir, but not its contents

(add-hook 'magit-mode-hook 'pd/turn-off-show-trailing-whitespace)
(add-hook 'magit-log-edit-mode-hook 'pd/add-magit-commit-keybindings)

(defun pd/add-magit-commit-keybindings ()
  (define-key magit-log-edit-mode-map (kbd "C-x m S") 'pd/magit-insert-submodule-summary)
  (define-key magit-log-edit-mode-map (kbd "C-x m s") 'pd/magit-insert-signoff)
  (define-key magit-log-edit-mode-map (kbd "C-x m a") 'pd/magit-insert-author))

(defun pd/magit-insert-submodule-summary ()
  "Insert the contents of `git submodule summary` into the current buffer"
  (interactive)
  (let ((summary (magit-git-string "submodule" "summary")))
    (if summary
      (save-excursion
        (goto-char (point-max))
        (insert "\n" summary))
      (message "No submodule summary available"))))

(defun pd/magit-insert-author (author)
  (interactive (list (ido-completing-read "Author: " (pd/magit-authors-and-committers-in-repo))))
  (save-excursion
    (goto-char (point-min))
    (magit-log-edit-set-field 'author author)))

(defun pd/magit-insert-signoff (signer)
  (interactive (list (ido-completing-read "Signed-off-by: " (pd/magit-authors-and-committers-in-repo))))
  (save-excursion
    (goto-char (point-max))
    (insert "\nSigned-off-by: " signer)))

(defun pd/magit-authors-and-committers-in-repo ()
  (let ((output (magit-git-string "log" "--pretty=format:%an <%ae>%n%cn <%ce>")))
    (pd/uniq (split-string output "[\r\n]+"))))
