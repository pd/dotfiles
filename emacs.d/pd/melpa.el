(defvar melpa-dir (file-name-as-directory (expand-file-name "~/vendor/melpa/")))
(add-to-list 'load-path melpa-dir)
(require 'package-build)

(defun pd/browse-melpa-package-homepage ()
  "Open github / emacswiki homepage of the MELPA package in region."
  (interactive)
  (let* ((package (if mark-active (buffer-substring (region-beginning) (region-end))
                   (read-string "MELPA Package: ")))
         (recipe  (cdr (pb/read-recipe (format "%s/recipes/%s" melpa-dir package))))
         (fetcher (plist-get recipe :fetcher))
         (repo    (plist-get recipe :repo)))
    (cond
     ((eq 'github fetcher) (browse-url (format "https://github.com/%s" repo)))
     (t (message "Only supports github for right now.")))))

; (add-hook 'package-menu-mode-hook)
