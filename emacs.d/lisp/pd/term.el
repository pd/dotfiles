;; -*- lexical-binding: t; -*-

(defun pd/is-term (buf)
  (with-current-buffer buf
    (or
     (eq major-mode 'ghostel-mode)
     (eq major-mode 'vterm-mode))))

(defun pd/term-buffers ()
  (seq-filter #'pd/is-term (buffer-list)))

(defvar consult-term-buffer-source
  `(:name "term"
          :hidden   nil
          :narrow   ?t
          :category buffer
          :state    ,#'consult--buffer-state
          :items    ,(lambda () (mapcar #'buffer-name (pd/term-buffers)))))

(defun pd/term-at (path)
  (interactive "fDir: \n")
  (let ((default-directory (if (file-directory-p path) path
                             (file-name-directory path))))
    (ghostel t)))

(defun pd/ssh-hosts ()
  "Extract Host block names from SSH configs."
  (let* ((parse-ssh-config
          (lambda (path)
            (let ((fname (expand-file-name path)))
              (when (file-exists-p fname)
                (mapcar 'cadr (tramp-parse-sconfig fname))))))

         (ssh-configs
          (apply 'append (mapcar parse-ssh-config '("~/.ssh/config" "~/.orbstack/ssh/config")))))
    (remq nil ssh-configs)))

(defun pd/term-on (host)
  "Launch terminal on HOST."
  (interactive
   (list (completing-read "Host: " (nconc '("localhost") (pd/ssh-hosts)))))
  (if (string-equal host "localhost")
      (pd/term-at "~")
    (pd/term-at (format "/ssh:%s:." host))))

(defun pd/consult-term (&optional arg)
  "Use consult to switch to a terminal buffer.
With prefix arg, or if no terms exist, create a new one in default-directory."
  (interactive "P")
  (require 'consult)
  (let* ((terms (pd/term-buffers))
         (in-term (pd/is-term (current-buffer)))
         (n (length terms)))
    (cond
     ((or arg (eq n 0))  ;; no terminals, or explicit prefix arg
      (ghostel arg))     ;; open a terminal

     ((eq n 1)                           ;; with one terminal
      (if (not in-term)                  ;; if we're not in it,
          (switch-to-buffer (car terms)) ;; then switch to it;
        (ghostel t)))                    ;; else open a new one.

     (t ;; otherwise just let consult filter it
      (consult--multi '(consult-term-buffer-source))))))

(provide 'pd/term)
