(eval-when-compile
  (require 'eproject)
  (require 'linkify))

;; Make sure this is available globally
(setq-default olympian-run-cmd nil)
(make-variable-buffer-local 'olympian-run-cmd)

;; How to determine if we're in an olympian project
(define-project-type oly-app (generic)
  (eproject--scan-parents-for file
                              (lambda (dir)
                                (or (string-match "oly-dev/$" (file-name-directory dir)) nil))))

;; When we are, we get special keybindings
(add-hook 'oly-app-project-file-visit-hook
          (lambda ()
            (define-key eproject-mode-map (kbd "C-c C-o a RET") 'olympian-run-aok)
            (define-key eproject-mode-map (kbd "C-c C-o a s") 'olympian-run-aok:specs)
            (define-key eproject-mode-map (kbd "C-c C-o a i") 'olympian-run-aok:integration)
            (define-key eproject-mode-map (kbd "C-c C-o a f") 'olympian-run-aok:features)
            (define-key eproject-mode-map (kbd "C-c C-o l") 'olympian-tail-log)
            (define-key eproject-mode-map (kbd "C-c C-o f RET") 'olympian-run-feature)
            (define-key eproject-mode-map (kbd "C-c C-o f .") 'olympian-run-feature-at-line)
            ; Overrides my typical ruby irb keybinding
            (define-key eproject-mode-map (kbd "C-c r i") 'olympian-run-script-console)))

;; Implementation
(defun olympian-app-root ()
  (file-name-as-directory eproject-root))

(defmacro olympian-in-app-root (&rest body)
  `(let ((default-directory ,(olympian-app-root)))
     ,@body))

; Use script/console, not irb
(defun olympian-run-script-console ()
  (interactive)
  (olympian-in-app-root
   (run-ruby "script/console")))

(defun olympian-ansi-linkify-proc-filter (proc string)
  (linkify-filter proc (ansi-color-apply string)))

(defun olympian-run (bufname cmd args)
  "Uses start-process to run CMD with ARGS, with output to buffer BUFNAME"
  (setq buf (get-buffer-create bufname))
  (save-excursion
    (set-buffer buf)
    (erase-buffer)
    (setq linkify-regexps '(" ?\\(/?[a-zA-z_/0-9\\.-]+\\):\\([0-9]+\\)"))
    (olympian-store-rerun-infos cmd args)
    (local-set-key (kbd "C-c C-r") 'olympian-rerun))
  (olympian-run-with-ansi-linkify buf cmd args)
  (display-buffer buf))

(defun olympian-run-with-ansi-linkify (buf cmd args)
  (set-process-filter
   (apply #'start-process (concat "olympian: " cmd) buf cmd args)
   'olympian-ansi-linkify-proc-filter))

(defun olympian-store-rerun-infos (cmd args)
  (setq olympian-run-cmd (list cmd args)))

(defun olympian-rerun ()
  "Reruns the command used in an olympian-run buffer"
  (interactive)
  (let ((cmd  (car  olympian-run-cmd))
        (args (cadr olympian-run-cmd))
        (buf  (buffer-name (current-buffer))))
    (olympian-run buf cmd args)))

(defun olympian-rake (task)
  (olympian-in-app-root
   (olympian-run (concat "*oly: rake " task "*")
                 "rake" (list task))))

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

(defun olympian-tail-log (log)
  "Opens a buffer, tailing the named log; defaults to development.log"
  (interactive "sLog (default development): ")
  (let ((log (concat (if (string= "" log)
                         "development"
                       log) ".log")))
    (olympian-in-app-root
     (olympian-run (concat "*oly: " log "*")
                   "tail" (list "-f" (concat "log/" log))))))

(defun olympian-run-cucumber (&rest args)
  "Runs cucumber with from the application root, using the default profile,
with arguments ARGS"
  (interactive)
  (olympian-in-app-root
   (olympian-run "*oly: cucumber*"
                 "script/cucumber" (nconc (list "-p" "default") args))))

(defun olympian-run-feature ()
  "Runs the current buffer's file with cucumber"
  (interactive)
  (olympian-run-cucumber (buffer-file-name)))

(defun olympian-run-feature-at-line ()
  "Runs the scenario beneath the cursor with cucumber"
  (interactive)
  (olympian-run-cucumber (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
