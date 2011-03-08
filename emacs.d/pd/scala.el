(require 'scala-mode-auto)
(require 'ensime)

(defvar pd/scala-hook nil)

(add-hook 'pd/scala-hook 'pd/run-coding-hook)

(defun pd/run-scala-hook ()
  (run-hooks 'pd/scala-hook))

(add-hook 'scala-mode-hook 'pd/run-scala-hook)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'pd/scala)
