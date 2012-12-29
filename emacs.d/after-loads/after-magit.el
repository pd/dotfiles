(setq magit-save-some-buffers nil
      magit-completing-read 'ido-completing-read
      magit-omit-untracked-dir-contents nil)

(add-hook 'magit-mode-hook 'pd/turn-off-show-trailing-whitespace)

(keydef (magit-log-edit "C-x m S") pd/magit-insert-submodule-summary)
(keydef (magit-log-edit "C-x m s") pd/magit-insert-signoff)
(keydef (magit-log-edit "C-x m a") pd/magit-insert-author)

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
    (-uniq (split-string output "[\r\n]+"))))
