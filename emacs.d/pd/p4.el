(when (file-exists-p "~/.p4config")
  (require 'p4)
  (setenv "P4CONFIG" (file-truename "~/.p4config")))

(provide 'pd/p4)
