(require 'dired-details+)

(setq-default dired-details-hidden-string "--- ")
(when (pd/macosx-p)
  (setq dired-use-ls-dired nil))

(dired-details-install)
