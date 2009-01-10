(eval-when-compile (require 'linkify))

(add-hook 'oly-app-project-file-visit-hook
          (lambda ()
            (define-key eproject-mode-map (kbd "C-c o a") 'olympian-run-aok)
            (define-key eproject-mode-map (kbd "C-c o s") 'olympian-run-aok:specs)
            (define-key eproject-mode-map (kbd "C-c o i") 'olympian-run-aok:integration)
            (define-key eproject-mode-map (kbd "C-c o f") 'olympian-run-aok:features)))

(defun olympian-ansi-linkify-proc-filter (proc string)
  (linkify-filter proc (ansi-color-apply string)))

(defun olympian-run (bufname cmd args)
  "Uses start-process to run CMD with ARGS, with output to buffer BUFNAME"
  (setq buf (get-buffer-create bufname))
  (save-excursion
    (set-buffer buf)
    (erase-buffer)
    (setq linkify-regexps
          '("^\\(/.*\\):\\([0-9]+\\)$"
            " \\(features/.+\\):\\([0-9]+\\)")))
  (setq proc (apply #'start-process (concat "olympian: " cmd) buf cmd args))
  (set-process-filter proc 'olympian-ansi-linkify-proc-filter)
  (display-buffer buf))

(defun olympian-rake (task)
  (olympian-run (concat "oly: rake " task)
                "rake" (list task)))

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
