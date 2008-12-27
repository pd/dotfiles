(require 'project-root)
(require 'jump)

(defun project-root-or-current-directory ()
  (unless project-details (project-root-fetch))
  (or (cdr project-details) default-directory))

(defun reverse-alist (alist)
  (mapcar
   (lambda (cell) (cons (cdr cell) (car cell)))
   alist))

; Disable inflection of the regexp matches; this probably breaks
; rinari's use of jump-inflections, but I don't use it yet.
(defun jump-inflections (terms) (cons terms nil))

(setq rspec-impl-to-spec-file-map
      '(("app/\\1.rb" . "spec/\\1_spec.rb")
	("lib/\\1.rb" . "spec/lib/\\1_spec.rb")
	("plugins/\\1/app/\\2/\\3.rb" . "plugins/\\1/spec/\\2/\\3_spec.rb")
	("plugins/\\1/app/\\2/\\3.rb" . "plugins/\\1/spec/\\2/integration/\\3_spec.rb")
	("plugins/\\1/lib/\\2.rb"     . "plugins/\\1/spec/lib/\\2_spec.rb")
	("plugins/\\1/lib/\\2.rb"     . "plugins/\\1/spec/lib/integration/\\2_spec.rb")))
(setq rspec-spec-to-impl-file-map
      (reverse-alist rspec-impl-to-spec-file-map))

(defjump 'jump-to-spec-file
  rspec-impl-to-spec-file-map
  'project-root-or-current-directory
  "Jump from an implementation to its spec")

; I really just want the jump specs here to be the ALIST from above,
; but with key/value swapped. But an hour of attempts ended in failure.
(defjump 'jump-to-implementation-file
  rspec-spec-to-impl-file-map
  'project-root-or-current-directory
  "Jump from a spec to its implementation")

(global-set-key (kbd "C-c j s") 'jump-to-spec-file)
(global-set-key (kbd "C-c j i") 'jump-to-implementation-file)

(provide 'my-jumps)
