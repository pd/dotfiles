;; adds a hook to keep track of the last few file-visiting buffers
;; I've killed, so I can quickly restore it, as I have a consistently
;; itchy C-x k finger. simpler than trying to coerce recentf or
;; similar into doing this for me.

(require 'dash)

(defvar pd/kill-tracker-list nil
  "The last N files referenced by a killed buffer; most recently
killed is at the front of the list.")

(defvar pd/kill-tracker-list-max-length 10
  "How many recently killed files to track.")

(defun pd/kill-tracker-add ()
  (let ((file (buffer-file-name)))
    (when file
      (setq pd/kill-tracker-list
            (-take pd/kill-tracker-list-max-length (cons file pd/kill-tracker-list))))))

(defun pd/kill-tracker-find-last ()
  (interactive)
  (let ((file (car pd/kill-tracker-list)))
    (when (and file (file-exists-p file))
      (setq pd/kill-tracker-list (cdr pd/kill-tracker-list))
      (find-file file))))

(add-hook 'kill-buffer-hook 'pd/kill-tracker-add)

(provide 'pd/kill-tracker)
