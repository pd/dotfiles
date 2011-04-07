; load all .el files in a directory. this does not descend.
(defun pd/load-directory (path &optional noerror nomessage)
  (dolist (file (directory-files path 'full "\\.el\\'"))
    (load file noerror nomessage)))

; is my private emacs.d available?
(defun pd/has-private-emacsd-p ()
  (file-exists-p "~/dotfiles/private/emacs.d/init.el"))

(defun pd/load-private-emacsd ()
  (add-to-list 'load-path "~/dotfiles/private/emacs.d")
  (load "~/dotfiles/private/emacs.d/init.el"))

; kill this buffer, then immediately reopen it where i was.
; useful for when mode hooks have been updated and i want them rerun.
; from xale@#emacs
(defun reload-buffer ()
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

; urgency hints for x11
; stolen from http://www.emacswiki.org/emacs/JabberEl#toc16
(defun pd/x-urgency-hint (frame arg &optional source)
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

; ansi-term creation
(defun pd/term-buffer-name ()
  (find-if (lambda (b)
             (save-excursion
               (set-buffer b)
               (string= "term-mode" (symbol-name major-mode))))
           (buffer-list)))

(defun pd/term-buffer-exists ()
  (not (eq ()
           (pd/term-buffer-name))))

(defun pd/term (create-new)
  (interactive "P")
  (let ((current-term (pd/term-buffer-name)))
    (if (and current-term (not create-new))
        (switch-to-buffer current-term)
      (ansi-term (or (getenv "SHELL") "/bin/zsh")))))

; ssh in an ansi-term
(defun pd/ssh (user host)
  (let* ((dest (concat user "@" host))
         (buf (apply 'make-term (generate-new-buffer-name dest) "ssh" nil (list dest))))
    (switch-to-buffer buf)))

; psql in a comint buffer
(defun pd/psql (host port db user)
  (switch-to-buffer
   (make-comint (format "PSQL %s@%s" db host)
                "psql" nil "-U" user "-h" host "-p" port "--pset" "pager=off" db)))

(provide 'pd/defuns)
