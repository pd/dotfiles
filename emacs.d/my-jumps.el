(require 'project-root)
(require 'jump)

(defun project-root-or-current-directory ()
  (unless project-details (project-root-fetch))
  (or (cdr project-details) default-directory))

(defjump 'jump-to-spec-file
  '(("app/\\1/\\2.rb" . "spec/\\1/\\2_spec.rb")
    ("app/\\1/\\2/\\3.rb" . "spec/\\1/\\2/\\3_spec.rb")
    ("lib/\\1.rb"     . "spec/lib/\\1_spec.rb"))
  'project-root-or-current-directory
  "Jump from an implementation to its spec")

(defjump 'jump-to-implementation-file
  '(("spec/\\1/\\2/\\3_spec.rb" . "app/\\1/\\2/\\3.rb")
    ("spec/\\1/\\2_spec.rb" . "app/\\1/\\2.rb"))
  'project-root-or-current-directory
  "Jump from a spec to its implementation")

(global-set-key (kbd "C-c j s") 'jump-to-spec-file)
(global-set-key (kbd "C-c j i") 'jump-to-implementation-file)

(provide 'my-jumps)
