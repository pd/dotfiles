(require 'project-root)
(require 'jump)

(defun project-root-or-current-directory ()
  (unless project-details (project-root-fetch))
  (or (cdr project-details) default-directory))

(defjump 'jump-to-spec-file
  '(("app/\\1.rb" . "spec/\\1_spec.rb")
    ("lib/\\1.rb" . "spec/lib/\\1_spec.rb")
    ("plugins/\\1/app/\\2/\\3.rb" . "plugins/\\1/spec/\\2/\\3_spec.rb")
    ("plugins/\\1/app/\\2/\\3.rb" . "plugins/\\1/spec/\\2/integration/\\3_spec.rb"))
  'project-root-or-current-directory
  "Jump from an implementation to its spec")

; I really just want the jump specs here to be the ALIST from above,
; but with key/value swapped. But an hour of attempts ended in failure.
(defjump 'jump-to-implementation-file
  '(("spec/\\1/\\2/\\3_spec.rb" . "app/\\1/\\2/\\3.rb")
    ("spec/\\1/\\2_spec.rb" . "app/\\1/\\2.rb")
    ("spec/lib/\\1_spec.rb" . "lib/\\1.rb"))
  'project-root-or-current-directory
  "Jump from a spec to its implementation")

(global-set-key (kbd "C-c j s") 'jump-to-spec-file)
(global-set-key (kbd "C-c j i") 'jump-to-implementation-file)

(provide 'my-jumps)
