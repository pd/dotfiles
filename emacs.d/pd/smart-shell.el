;; smarter shell creation
(defun pd/smart-shell ()
  "If currently in a shell-mode buffer, restart the shell if it has exited.
Otherwise, start a new shell.

If not in shell-mode and one shell-mode buffer exists, switch to it.
If more than one shell-mode buffer exists, switch to the nearest one,
according to pd/nearest-shell."
  (interactive)
  (let ((next-shell-name (generate-new-buffer-name "*shell*"))
        (in-shell-buffer (eq 'shell-mode major-mode))
        (process-alive-p (eq nil (get-buffer-process (current-buffer))))
        (wants-new-shell (not (eq nil current-prefix-arg))))
    (if in-shell-buffer
        (if process-alive-p (shell) (shell next-shell-name))
      (switch-to-buffer (if wants-new-shell (shell next-shell-name)
                          (or (pd/nearest-shell) (shell next-shell-name)))))))

(defun pd/shell-buffer-list ()
  "Returns a list of all shell buffers"
  (remove-if-not (lambda (buf)
                   (eq 'shell-mode (with-current-buffer buf major-mode)))
                 (buffer-list)))

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

(provide 'pd/smart-shell)
