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
(defun coding-hook ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode)
  (highlight-warning-words))
