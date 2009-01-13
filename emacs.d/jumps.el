; Disable inflection of the regexp matches; this probably breaks
; rinari's use of jump-inflections, but I don't use it yet.
(defun jump-inflections (terms) (cons terms nil))

(setq rspec-impl-to-spec-file-map
      '(("app/\\1.rb" . "spec/\\1_spec.rb")
	("lib/\\1.rb" . "spec/lib/\\1_spec.rb")
	("lib/\\1.rb" . "spec/\\1_spec.rb")
	("plugins/\\1/app/\\2/\\3.rb" . "plugins/\\1/spec/\\2/\\3_spec.rb")
	("plugins/\\1/app/\\2/\\3.rb" . "plugins/\\1/spec/\\2/integration/\\3_spec.rb")
	("plugins/\\1/lib/\\2.rb"     . "plugins/\\1/spec/lib/\\2_spec.rb")
	("plugins/\\1/lib/\\2.rb"     . "plugins/\\1/spec/lib/integration/\\2_spec.rb")))
(setq rspec-spec-to-impl-file-map
      (swap-alist-pairs rspec-impl-to-spec-file-map))

(defjump 'rspec-jump-to-spec-file
  rspec-impl-to-spec-file-map
  'project-root-or-current-directory
  "Jump from an implementation to its spec")

(defjump 'rspec-jump-to-implementation-file
  rspec-spec-to-impl-file-map
  'project-root-or-current-directory
  "Jump from a spec to its implementation")
