(defun pd/find-init.el ()
  "find-file init.el"
  (interactive)
  (find-file (expand-file-name user-init-file)))

(defun pd/emacs.d (&rest paths)
  (expand-file-name (s-join "/" paths) user-emacs-directory))

(defun pd/load-ext (name)
  "Load extensions to a libary from `pd/ext/NAME.el'"
  (let* ((name (if (symbolp name) (symbol-name name) name))
         (dir  (expand-file-name "pd/ext/" user-emacs-directory))
         (file (concat name ".el")))
    (load (expand-file-name file dir))))

(defun pd/open-line-after ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun pd/open-line-before ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun pd/join-next-line ()
  "Join current and next line."
  (interactive)
  (join-line 1))

(defun pd/rename-buffer-and-file ()
  "Renames current buffer and its file."
  (interactive)
  (let ((name (buffer-name))
        (file (buffer-file-name)))
    (cond
     ((not file) (error "Buffer '%s' is not visiting a file." name))
     ((not (file-exists-p file)) (error "File '%s' does not exist." file))
     (t (let ((new-name (ido-read-file-name "Rename to: " nil file)))
          (rename-file file new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun pd/kill-file-name ()
  "Put the current filename's expanded path into the kill-ring and clipboard.
Returns the filename."
  (interactive)
  (let* ((filename (or (buffer-file-name) dired-directory))
         (path     (when filename (expand-file-name filename))))
    (when path
      (with-temp-buffer
        (insert path)
        (kill-region (point-min) (point-max)))
      path)))

(defun pd/message-file-name ()
  "Run `pd/kill-file-name' and use `message' to display the result."
  (interactive)
  (message "%s" (pd/kill-file-name)))

(defun pd/reload-buffer ()
  "Kill the current buffer and immediately reload it without moving point.

Useful for rerunning mode hooks."
  (interactive)
  (let ((path (buffer-file-name)) (point (point)))
    (kill-buffer)
    (find-file path)
    (goto-char point)))


(provide 'pd/defuns)
