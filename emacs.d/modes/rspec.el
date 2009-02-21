(autoload 'rspec-mode "rspec-mode" "Major mode for rspec" t)
(autoload 'rspec-run-suite "rspec-mode" "" t)

(add-to-list 'auto-mode-alist '("_spec.rb\\'" . rspec-mode))

(add-hook 'rspec-mode-hook
          (lambda ()
            (define-key rspec-mode-map (kbd "C-c s s") 'rspec-run-suite)
            (define-key rspec-mode-map (kbd "C-c s RET") 'rspec-run-examples)
            (define-key rspec-mode-map (kbd "C-c s .") 'rspec-run-example)
            (define-key rspec-mode-map (kbd "C-c s h") 'rspec-require-spec-helper)))

(defun string-n-times (string n)
  (let ((s ""))
    (while (> n 0)
      (setf s (concat s string))
      (decf n))
    s))

(defun rspec-parent-directory (dir)
  (file-name-directory (directory-file-name dir)))

(defun rspec-is-root-directory-p (dir)
  (equal dir (rspec-parent-directory dir)))

(defun rspec-distance-to-spec-helper (dir &optional steps)
  (setf steps (or steps 0))
  (if (directory-files dir t "^spec_helper\\.rb$")
      steps
    (unless (rspec-is-root-directory-p dir)
      (rspec-distance-to-spec-helper (rspec-parent-directory dir) (+ steps 1)))))

(defun rspec-require-spec-helper ()
  (interactive)
  (let ((dist (rspec-distance-to-spec-helper (file-name-directory (buffer-file-name)))))
    (when dist
      (insert "require File.expand_path(File.dirname(__FILE__) + '"
              (string-n-times "/.." dist)
              "/spec_helper')"))))
