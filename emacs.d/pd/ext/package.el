(require 's)

(defun pd/package-upgrades ()
  "More useful than the '12 packages can be upgraded' message package.el offers."
  (interactive)
  (let ((upgrades (package-menu--find-upgrades)))
    (when upgrades (message "Outdated: %s" (s-join ", " (mapcar 'symbol-name (mapcar 'car upgrades)))))))
