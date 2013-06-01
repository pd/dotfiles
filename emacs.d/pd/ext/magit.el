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
