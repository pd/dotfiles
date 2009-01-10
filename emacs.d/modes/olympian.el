(eval-when-compile (require 'linkify))

(define-minor-mode olympian-mode
  "Minor mode for Olympian projects"
  :lighter " oly-dev"
  :keymap (let ((km (make-sparse-keymap)))
            (define-key km (kbd "C-c o a") 'olympian-run-aok)
            (define-key km (kbd "C-c o s") 'olympian-run-aok:specs)
            (define-key km (kbd "C-c o i") 'olympian-run-aok:integration)
            (define-key km (kbd "C-c o f") 'olympian-run-aok:features)
            km))

(add-hook 'find-file-hook
          (lambda ()
            (if (string-match "oly-dev/" (expand-file-name (buffer-file-name)))
                (olympian-mode))))

(defun olympian-ansi-linkify-proc-filter (proc string)
  (linkify-filter proc (ansi-color-apply string)))

(defun olympian-rake (task)
  (setq rake-results (get-buffer-create "rake-results"))
  (save-excursion
    (set-buffer rake-results)
    (erase-buffer)
    (setq linkify-regexps
          '("^\\(/.*\\):\\([0-9]+\\)$"
            " \\(features/.+\\):\\([0-9]+\\)")))
  (setq proc (apply #'start-process (concat "rake " task) rake-results "rake" (list task)))
  (set-process-filter proc 'olympian-ansi-linkify-proc-filter)
  (display-buffer rake-results))

(defun olympian-run-aok ()
  (interactive)
  (olympian-rake "aok"))

(defun olympian-run-aok:specs ()
  (interactive)
  (olympian-rake "aok:specs"))

(defun olympian-run-aok:integration ()
  (interactive)
  (olympian-rake "aok:integration"))

(defun olympian-run-aok:features ()
  (interactive)
  (olympian-rake "aok:features"))
