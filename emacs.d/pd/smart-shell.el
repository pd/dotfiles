;; smarter shell creation
(defun pd/smart-shell ()
  "If currently in a shell-mode buffer, restart the shell if it has exited.
Otherwise, start a new shell.

If not in shell-mode and one shell-mode buffer exists, switch to it.
If more than one shell-mode buffer exists, switch to the nearest one,
according to pd/nearest-shell."
  (interactive)
  (let ((next-shell-name (generate-new-buffer-name "*shell*"))
        (process-alive-p (null (get-buffer-process (current-buffer))))
        (wants-new-shell (when current-prefix-arg t)))
    (if (pd/smart-shell-p (current-buffer))
        (if process-alive-p (shell) (shell next-shell-name))
      (switch-to-buffer
       (if wants-new-shell
           (shell next-shell-name)
         (or (pd/nearest-shell) (shell next-shell-name)))))))

(defun pd/shell-buffer-list ()
  "Returns a list of all shell buffers"
  (remove-if-not 'pd/smart-shell-p (buffer-list)))

(defun pd/distance-to-buffer (buffer)
  "Returns the levenshtein distance between BUFFER's directory and this buffer's directory"
  (require 'levenshtein)
  (levenshtein-distance (with-current-buffer buffer default-directory)
                        default-directory))

(defun pd/nearest-shell ()
  "Returns the shell buffer whose default-directory is closest
to the default-directory of the current buffer."
  (car (sort* (pd/shell-buffer-list) '< :key 'pd/distance-to-buffer)))

(defun pd/ido-switch-shell ()
  "Handy wrapper around ido-switch-buffer which only lists shell-mode buffers."
  (interactive)
  (let* ((shell-names (mapcar 'buffer-name (pd/shell-buffer-list)))
         (buffer-name (ido-completing-read "Buffer: " shell-names)))
    (when buffer-name (switch-to-buffer buffer-name))))

(defun pd/smart-shell-p (buf)
  "Returns whether the given buffer is considered an active shell-mode buffer."
  (and (eq 'shell-mode (with-current-buffer buf major-mode))
       (string-match-p "^\*shell" (buffer-name buf))))

(provide 'pd/smart-shell)
