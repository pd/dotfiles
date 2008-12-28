(require 'project-root)

(defun project-root-or-current-directory ()
  (unless project-details (project-root-fetch))
  (or (cdr project-details) default-directory))

(defun reverse-alist-pairs (alist)
  (mapcar
   (lambda (cell) (cons (cdr cell) (car cell)))
   alist))

(defun highlight-warning-words ()
  (progn
    (font-lock-add-keywords
     nil '(("\\<\\(TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
	    1 font-lock-warning-face t)))
    nil))

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
