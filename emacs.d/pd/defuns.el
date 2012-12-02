; filter a list. but i never remember the remove-if-not name,
; so name it something i don't forget
(defalias 'pd/filter 'remove-if-not)
(defalias 'pd/reject 'remove-if)

(defun pd/uniq (list)
  "Remove duplicate elements from a list"
  (let ((list list))
    (while list
      (setq list (setcdr list (delete (car list) (cdr list))))))
  list)

(defun pd/load-directory (path &optional noerror nomessage)
  "Load all .el files in the given directory. Non-recursive."
  (dolist (file (directory-files path 'full "\\.el\\'"))
    (load file noerror nomessage)))

(defun pd/login-shell-path ()
  "Launch a login shell and return its $PATH as a list"
  (let* ((result (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))
         (trimmed (replace-regexp-in-string "[[:space:]\n]*$" "" result)))
    (split-string trimmed path-separator)))

(defun pd/eval-url (url)
  "Load url and eval its contents as an Emacs Lisp script"
  (let ((buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (eval-region (point) (point-max))
      (kill-buffer (current-buffer)))))

(defun pd/has-private-emacsd-p ()
  "t if my private dotfiles init.el is available"
  (file-exists-p "~/dotfiles/private/emacs.d/init.el"))

(defun pd/load-private-emacsd ()
  "add private emacs.d to load-path and load the init.el"
  (add-to-list 'load-path "~/dotfiles/private/emacs.d")
  (load "~/dotfiles/private/emacs.d/init.el"))

; from xale@#emacs
(defun reload-buffer ()
  "Kill the current buffer and immediately reload it without moving point.
Useful for rerunning mode hooks."
  (interactive)
  (let ((path (buffer-file-name)) (point (point)))
    (kill-buffer)
    (find-file path)
    (goto-char point)))

; superior mark handling
; stolen from http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/
(defun pd/push-mark ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun pd/jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

; vi's o and O
(defun pd/insert-newline ()
  (funcall (or (local-key-binding (kbd "<return>"))
               (key-binding (kbd "RET")))))

(defun pd/append-and-move-to-new-line ()
  "Inserts a blank line after the current one, and moves to it"
  (interactive)
  (end-of-line)
  (pd/insert-newline))

(defun pd/prepend-and-move-to-new-line ()
  "Inserts a blank line before the current one, and moves to it"
  (interactive)
  (if (= 1 (line-number-at-pos))
      (progn
        (beginning-of-buffer)
        (pd/insert-newline)
        (beginning-of-buffer))
    (progn
      (previous-line)
      (pd/append-and-move-to-new-line))))

; ido support for recentf; stolen from emacs-starter-kit
(defun pd/recentf-ido-find-file ()
  (interactive)
  (let ((file (ido-completing-read "Find recent: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun pd/x-urgency-hint (frame arg &optional source)
  "Signal an X11 urgency hint for this frame.
http://www.emacswiki.org/emacs/JabberEl#toc16"
  (let* ((wm-hints (append (x-window-property
			    "WM_HINTS" frame "WM_HINTS"
			    (if source
				source
			      (string-to-number
			       (frame-parameter frame 'outer-window-id)))
			    nil t) nil))
	 (flags (car wm-hints)))
    (setcar wm-hints
	    (if arg
		(logior flags #x00000100)
	      (logand flags #xFFFFFEFF)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun pd/modify-font-size (amount)
  "Increase/decrease the font size by amount"
  (set-face-attribute 'default nil
                      :height
                      (+ (face-attribute 'default :height)
                         amount)))

(defun pd/increase-font-size ()
  "Increase the font size by 20 (whatever the fuck that is)"
  (interactive)
  (pd/modify-font-size 20))

(defun pd/decrease-font-size ()
  "Decrease the font size by 20 (whatever the fuck that is)"
  (interactive)
  (pd/modify-font-size -20))

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

(defun pd/clear-shell ()
  "Clear the contents of a buffer without bothering with the kill-ring."
  (interactive)
  (delete-region (point-min) (point-max)))

;; ansi-term creation
(defun pd/term-buffer-name ()
  "Return the name of the first buffer in term-mode"
  (find-if (lambda (buf) (with-current-buffer buf (eq 'term-mode major-mode)))
           (buffer-list)))

(defun pd/term-buffer-exists ()
  "t if a term-mode buffer exists"
  (not (eq nil (pd/term-buffer-name))))

(defun pd/term (create-new)
  "Open an ansi-term running $SHELL (or /bin/zsh if undefined)"
  (interactive "P")
  (let ((current-term (pd/term-buffer-name)))
    (if (and current-term (not create-new))
        (switch-to-buffer current-term)
      (ansi-term (or (getenv "SHELL") "/bin/zsh")))))

(defun pd/ssh (user host)
  "Open a terminal buffer ssh'd to user@host"
  (interactive "MUser: \nMHost: ")
  (let* ((dest (concat user "@" host))
         (buf (apply 'make-term (generate-new-buffer-name dest) "ssh" nil (list dest))))
    (switch-to-buffer buf)))

(defun pd/psql (host port db user)
  "Open a comint buffer running psql"
  (switch-to-buffer
   (make-comint (format "PSQL %s@%s" db host)
                "psql" nil "-U" user "-h" host "-p" port "--pset" "pager=off" db)))

(defun pd/add-mode-to-first-line ()
  "Add the -*- mode: foo -*- line to the beginning of the current buffer"
  (interactive)
  (let ((mode-name (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode))))
    (save-excursion
      (beginning-of-buffer)
      (pd/prepend-and-move-to-new-line)
      (insert (concat "-*- mode: " mode-name " -*-"))
      (comment-region (point-min) (point)))))

(defun pd/macosx-p ()
  "t if on a darwin system"
  (string-equal "darwin" system-type))

(provide 'pd/defuns)
