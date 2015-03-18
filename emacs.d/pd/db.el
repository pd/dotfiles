(after 'edbi
  (when (require 'company-edbi nil 'noerror)
    (add-to-list 'company-backends 'company-edbi))
  (setq edbi:ds-history-file (locate-user-emacs-file ".crap/edbi-history")
        edbi:ds-history-list-num 50))

(defun pd/postgres-databases ()
  "Returns the list of database names available locally."
  (require 'pg)
  (with-pg-connection conn '("postgres" "postgres")
    (pg:databases conn)))

(defun pd/edbi ()
  "Open edbi on a database without having to remember how to construct DBI identifiers."
  (interactive)
  (require 'edbi)
  (let ((target-db-name (ido-completing-read "Database: " (pd/postgres-databases) nil t)))
    (when target-db-name
      (let (conn)
        (setq conn (edbi:start))
        (edbi:connect conn (edbi:data-source (concat "dbi:Pg:dbname=" target-db-name)))
        (deferred:call 'edbi:dbview-open conn)))))

(provide 'pd/db)
