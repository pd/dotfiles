(Given "^these temporary directories exist:$"
       (lambda (table)
         (mapcar (lambda (dirs)
                   (make-directory (concat "/tmp/" (car dirs)) 'parents))
                 table)))

(Given "^I am viewing the file \"\\(.+\\)\"$"
       (lambda (filename)
         (find-file filename)))

(Given "^I have a shell-mode buffer in \"\\(.+?\\)\" named \"\\(.+?\\)\""
       (lambda (dir buffer-name)
         (with-current-buffer (shell buffer-name)
           (cd dir))))

(Given "^I have a shell-mode buffer named \"\\(.+?\\)\"$"
       (lambda (buffer-name)
         (shell buffer-name)))

(Then "^the buffer major-mode should be \"\\(.+?\\)\"$"
      (lambda (expected-mode)
        (assert (equal (symbol-name major-mode) expected-mode))))

(Then "^I debug$"
      (lambda ()
        (message "%s" (list major-mode buffer-file-name default-directory))
        (message "%s" (pd/shell-buffer-list))))
