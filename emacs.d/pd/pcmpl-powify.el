(require 'pcomplete)
(require 'cl)

(defun pcmpl-powify-commands ()
  "Return a collection of all `powify' commands."
  '("server" "utils" "create" "destroy" "restart"
    "always_restart" "always_restart_off" "rename"
    "environment" "browse" "logs"))

(defun pcmpl-powify-server-names ()
  (let* ((pow-home (file-name-as-directory (expand-file-name "~/.pow")))
         (entries  (directory-files pow-home nil)))
    (remove-if (lambda (s) (string-match "^\\." s))
               entries)))

(defun pcomplete/powify ()
  "Completion rules for the `powify' command."
  (let (cmd)
    (pcomplete-here* (pcmpl-powify-commands))
    (setq cmd (pcomplete-arg 'last -1))
    (cond
     ((string= cmd "server")
      (pcomplete-here (pcmpl-powify-server)))
     ((or (string= cmd "restart")
          (string= cmd "always_restart")
          (string= cmd "always_restart_off")
          (string= cmd "logs"))
      (pcomplete-here (pcmpl-powify-server-names))))))

(provide 'pd/pcmpl-powify)
