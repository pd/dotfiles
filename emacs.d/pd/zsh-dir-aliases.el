(require 's)

;; zsh directory alias support
(defun pd/zsh-dir-aliases ()
  "Return list of zsh's dir aliases as (alias . expansion); or NIL if none."
  (let* ((aliases  nil)
         (zshout   (shell-command-to-string "zsh -i -c 'print -l -n ${(kv)nameddirs}'"))
         (zshlines (if (s-blank? zshout) nil (s-lines zshout))))
    (when (= 0 (% (length zshlines) 2))
      (while (> (length zshlines) 0)
        (push (cons (concat "~" (car zshlines)) (cadr zshlines))
              aliases)
        (setq zshlines (cddr zshlines)))
      aliases)))

(defvar pd/zsh-dir-aliases-cache (pd/zsh-dir-aliases)
  "A local cache of zsh's hash table for use in `pd/abbreviate-file-name'
and `pd/expand-file-name'. See also `pd/zsh-dir-aliases'.")

(defvar pd/directory-abbrev-alist
        (mapcar (lambda (alias) (cons (cdr alias) (car alias)))
                (pd/zsh-dir-aliases))
        "Reordering of `pd/zsh-dir-aliases' for use with `pd/abbreviate-file-name'.")

(defun pd/abbreviate-file-name (filename)
  "A version of `abbreviate-file-name' which replaces the
`directory-abbrev-alist' with my `pd/directory-abbrev-alist'."
  (let ((directory-abbrev-alist pd/directory-abbrev-alist))
    (abbreviate-file-name (expand-file-name filename))))

(defun pd/expand-file-name (filename)
  "Like `expand-file-name', but expands directory names based
on `pd/zsh-dir-aliases-cache'."
  (reduce (lambda (str alias)
            (s-replace (car alias) (cdr alias) str))
          pd/zsh-dir-aliases-cache
          :initial-value path))

(provide 'pd/zsh-dir-aliases)
