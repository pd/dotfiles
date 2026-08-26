;; -*- lexical-binding: t; -*-

(require 'project)

(defun pd/buffer-term-p (buf)
  (with-current-buffer buf
    (or
     (eq major-mode 'ghostel-mode)
     (eq major-mode 'vterm-mode))))

(defun pd/term-buffers ()
  (seq-filter #'pd/buffer-term-p (buffer-list)))

(defun pd/term-at (path)
  "Launch a terminal in PATH, or in its parent if PATH is a file."
  (interactive "DDir: ")
  (let* ((path (and path (substitute-in-file-name path)))
         (default-directory
          (cond
           ((or (null path) (string-blank-p path)) default-directory)
           ((file-directory-p path) path)
           (t (or (file-name-directory path) default-directory)))))
    (ghostel t)))

(defun pd/ssh-hosts ()
  "Extract Host block names from SSH configs."
  (require 'tramp)
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

(defvar pd/consult-term-buffer-source
  `(:name "term"
          :hidden   nil
          :default  t
          :narrow   ?t
          :category buffer
          :state    ,#'consult--buffer-state
          :new      ,#'pd/term-at
          :items    ,(lambda () (mapcar #'buffer-name (pd/term-buffers)))))

;; can't really do find-file style flow here so just hand off and
;; accept whatever it spits out
(defvar pd/consult-term-dir-source
  `(:name "dir"
          :hidden t
          :narrow ?d
          :action ,(lambda (_) (call-interactively #'pd/term-at))
          :new    ,#'pd/term-at
          :items  ("find directory...")))

(defvar pd/consult-term-project-source
  `(:name "project"
          :hidden   t
          :narrow   ?p
          :category file
          :face     consult-file
          :history  file-name-history
          :action   ,#'pd/term-at
          :new      ,#'pd/term-at
          :items    ,(lambda ()
                       (mapcar #'abbreviate-file-name
                               (seq-remove #'file-remote-p
                                           (project-known-project-roots))))))

(defvar pd/consult-term-host-source
  `(:name "host"
          :hidden  t
          :narrow  ?h
          :action  ,#'pd/term-on
          :new     ,#'pd/term-on
          :items   ,(lambda () (cons "localhost" (pd/ssh-hosts)))))

(defun pd/consult-term (&optional arg)
  "Use consult to switch to a terminal buffer.
With prefix arg, or if no terms exist, create a new one in default-directory.

Narrowing:
* `d': dirs
* `p': projects
* `h': hosts"
  (interactive "P")
  (require 'consult)
  (let* ((terms (pd/term-buffers))
         (in-term (pd/buffer-term-p (current-buffer)))
         (n (length terms)))
    (cond
     ((or arg (eq n 0))  ;; no terminals, or explicit prefix arg
      (ghostel arg))     ;; open a terminal

     ((eq n 1)                           ;; with one terminal
      (if (not in-term)                  ;; if we're not in it,
          (switch-to-buffer (car terms)) ;; then switch to it;
        (ghostel t)))                    ;; else open a new one.

     (t ;; otherwise just let consult filter it
      (consult--multi '(pd/consult-term-buffer-source
                        pd/consult-term-dir-source
                        pd/consult-term-project-source
                        pd/consult-term-host-source))))))

(provide 'pd/term)
