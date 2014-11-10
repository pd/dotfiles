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

; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
; tho everyone has had some version of this for ages. might as well use public code, tho.
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun pd/comment-dwim (arg)
  "If the region is active (`mark-active') and `transient-mark-mode'
is on, let's `comment-dwim' do its thing.

If not, `comment-dwim' doesn't DWIM at all. Instead, comment or
uncomment the current line."
  (interactive "*P")
  (if (and mark-active transient-mark-mode)
      (comment-dwim arg)
    (save-excursion
      (back-to-indentation)
      (let ((beg (point)))
        (end-of-line)
        (comment-or-uncomment-region beg (point))))))

(defun pd/dirtrack-directory-changed ()
  "Updates shell-mode buffer names to reflect their current directory."
  (when (and (eq 'shell-mode major-mode)
             (string-match-p "^\*shell" (buffer-name)))
    (let ((cwd (directory-file-name (abbreviate-file-name default-directory))))
      (rename-buffer (format "*shell: %s*" cwd) t))))

(defun pd/shell-switcher-switch-or-new-buffer (prefix)
  "C-u to unconditionally make a new buffer."
  (interactive "P")
  (if prefix
      (shell-switcher-new-shell)
    (shell-switcher-switch-buffer)))

(defun pd/reboot-theme ()
  (interactive)
  (let ((themes (reverse custom-enabled-themes)))
    (--each custom-enabled-themes (disable-theme it))
    (--each themes (enable-theme it))))

(provide 'pd/defuns)
