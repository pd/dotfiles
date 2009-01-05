(require 'eproject)

(defun project-root-or-current-directory ()
  (or eproject-root default-directory))

(defun swap-alist-pairs (alist)
  (mapcar
   (lambda (cell) (cons (cdr cell) (car cell)))
   alist))

(defun highlight-warning-words ()
  (progn
    (font-lock-add-keywords
     nil '(("\\<\\(TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
	    1 font-lock-warning-face t)))
    nil))

(defun lisp-eval-buffer ()
  (interactive)
  (lisp-eval-region (point-min) (point-max)))

(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                         'fullboth)))

(defun non-erc-buffer-list ()
  (save-excursion
    (delq nil
          (mapcar (lambda (buf)
                    (when (buffer-live-p buf)
                      (with-current-buffer buf
                        (and (not (eq major-mode 'erc-mode))
                             buf))))
                  (buffer-list)))))

(defun buffer-names (bufs)
  (mapcar (lambda (buf) (buffer-name buf))
          bufs))

(defmacro with-ido-buf-list (bufs &rest body)
  `(let ((ido-make-buffer-list-hook
          (lambda () (setq ido-temp-list ,bufs))))
     ,@body))

(defun ido-switch-buffer-erc ()
  (interactive)
  (with-ido-buf-list (buffer-names (erc-buffer-list))
                     (switch-to-buffer (ido-read-buffer "IRC: "))))

(defun ido-switch-buffer-non-erc ()
  (interactive)
  (with-ido-buf-list (buffer-names (non-erc-buffer-list))
                     (switch-to-buffer (ido-read-buffer "Buffer: "))))

(defun ido-kill-buffer-erc ()
  (interactive)
  (with-ido-buf-list (buffer-names (erc-buffer-list))
                     (kill-buffer (ido-read-buffer "Kill IRC buffer: "))))

(defun ido-kill-buffer-non-erc ()
  (interactive)
  (with-ido-buf-list (buffer-names (non-erc-buffer-list))
                     (kill-buffer (ido-read-buffer "Kill buffer: "))))

; From Pat Maddox
(defun append-and-move-to-new-line ()
  "Inserts a blank line after the current one, and moves to it"
  (interactive)
  (end-of-line)
  (funcall (or (local-key-binding (kbd "<return>")) (key-binding (kbd "RET")))))
(defun prepend-and-move-to-new-line ()
  "Inserts a blank line before the current one, and move to it"
  (interactive)
  (previous-line)
  (append-and-move-to-new-line))

; From emacs-starter-kit
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun coding-hook ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode)
  (highlight-warning-words))
