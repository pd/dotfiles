(eval-when-compile
  (require 'eproject)
  (require 'linkify))

;; Make sure this is available globally
(setq-default celect-ran-cmd nil)
(make-variable-buffer-local 'celect-ran-cmd)

;; How to determine if we're in an celect project
(define-project-type celect-app (generic)
  (eproject--scan-parents-for file
                              (lambda (dir)
                                (or (string-match "oly-dev/$" (file-name-directory dir))
                                    (string-match "celect/$"  (file-name-directory dir))
                                    nil))))

;; When we are, we get special keybindings
(add-hook 'celect-app-project-file-visit-hook
          (lambda ()
            ; rake aok:*
            (define-key eproject-mode-map (kbd "C-c C-c a RET") 'celect-run-aok)
            (define-key eproject-mode-map (kbd "C-c C-c a s") 'celect-run-aok:specs)
            (define-key eproject-mode-map (kbd "C-c C-c a i") 'celect-run-aok:integration)
            (define-key eproject-mode-map (kbd "C-c C-c a f") 'celect-run-aok:features)

            ; rspec
            (define-key eproject-mode-map (kbd "C-c C-c s RET") 'celect-run-spec)
            (define-key eproject-mode-map (kbd "C-c C-c s .") 'celect-run-spec-at-line)

            ; Cucumber
            (define-key eproject-mode-map (kbd "C-c C-c f RET") 'celect-run-feature)
            (define-key eproject-mode-map (kbd "C-c C-c f .") 'celect-run-feature-at-line)

            ; misc
            (define-key eproject-mode-map (kbd "C-c C-c l") 'celect-tail-log)

            ; Overrides my typical ruby irb keybinding
            (define-key eproject-mode-map (kbd "C-c r i") 'celect-run-script-console)))

;; Implementation
(defun celect-app-root ()
  (file-name-as-directory eproject-root))

(defmacro celect-in-app-root (&rest body)
  `(let ((default-directory ,(celect-app-root)))
     ,@body))

; Use script/console, not irb
(defun celect-run-script-console ()
  (interactive)
  (celect-in-app-root
   (run-ruby "script/console")))

(defun celect-ansi-linkify-proc-filter (proc string)
  (linkify-filter proc (ansi-color-apply string)))

(defun celect-run (bufname cmd args)
  "Uses start-process to run CMD with ARGS, with output to buffer BUFNAME"
  (setq buf (get-buffer-create bufname))
  (save-excursion
    (set-buffer buf)
    (erase-buffer)
    (setq linkify-regexps '(" ?\\(/?[a-zA-z_/0-9\\.-]+\\):\\([0-9]+\\)"))
    (celect-store-rerun-infos cmd args)
    (local-set-key (kbd "C-c C-r") 'celect-rerun))
  (celect-run-with-ansi-linkify buf cmd args)
  (display-buffer buf))

(defun celect-run-with-ansi-linkify (buf cmd args)
  (set-process-filter
   (apply #'start-process (concat "celect: " cmd) buf cmd args)
   'celect-ansi-linkify-proc-filter))

(defun celect-store-rerun-infos (cmd args)
  (setq celect-ran-cmd (list cmd args)))

(defun celect-rerun ()
  "Reruns the command used in an celect-run buffer"
  (interactive)
  (let ((cmd  (car  celect-ran-cmd))
        (args (cadr celect-ran-cmd))
        (buf  (buffer-name (current-buffer))))
    (celect-run buf cmd args)))

(defun celect-rake (task)
  (celect-in-app-root
   (celect-run (concat "*celect: rake " task "*")
                 "rake" (list task))))

(defun celect-run-aok ()
  (interactive)
  (celect-rake "aok"))

(defun celect-run-aok:specs ()
  (interactive)
  (celect-rake "aok:specs"))

(defun celect-run-aok:integration ()
  (interactive)
  (celect-rake "aok:integration"))

(defun celect-run-aok:features ()
  (interactive)
  (celect-rake "aok:features"))

(defun celect-tail-log (log)
  "Opens a buffer, tailing the named log; defaults to development.log"
  (interactive "sLog (default development): ")
  (let ((log (concat (if (string= "" log)
                         "development"
                       log) ".log")))
    (celect-in-app-root
     (celect-run (concat "*celect: " log "*")
                   "tail" (list "-f" (concat "log/" log))))))

(defun celect-run-rspec (&rest args)
  "Runs specs from the application root, using spec.opts, with arguments ARGS"
  (interactive)
  (celect-in-app-root
     (celect-run "*celect: spec*"
                 "script/spec" (nconc (list "-O" "spec/spec.opts") args))))

(defun celect-run-spec ()
  "Runs the current buffer's specs"
  (interactive)
  (celect-run-rspec (buffer-file-name)))

(defun celect-run-spec-at-line ()
  "Runs the spec at point"
  (interactive)
  (celect-run-rspec (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))

(defun celect-run-cucumber (&rest args)
  "Runs cucumber from the application root, using the default profile,
with arguments ARGS"
  (interactive)
  (celect-in-app-root
   (celect-run "*celect: cucumber*"
               "script/cucumber" (nconc (list "-p" "default") args))))

(defun celect-run-feature ()
  "Runs the current buffer's file with cucumber"
  (interactive)
  (celect-run-cucumber (buffer-file-name)))

(defun celect-run-feature-at-line ()
  "Runs the scenario beneath the cursor with cucumber"
  (interactive)
  (celect-run-cucumber (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
