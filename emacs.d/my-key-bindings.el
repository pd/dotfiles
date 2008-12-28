(require 'my-defuns)

; General keybindings
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-SPC") 'set-mark-command) ; Quicksilver is C-SPC

(global-set-key (kbd "M-<return>") 'append-and-move-to-new-line)
(global-set-key (kbd "M-S-<return>") 'prepend-and-move-to-new-line)

(global-set-key (kbd "<f6>") 'linum-mode)

; Bindings from emacs-starter-kit that were reasonable enough
(global-set-key (kbd "C-c v") 'eval-buffer)
(global-set-key (kbd "C-h a") 'apropos) ; defaults to command-apropos
(global-set-key (kbd "C-/") 'hippie-expand) ; keep M-/ around for now, I suppose

(windmove-default-keybindings) ; S-<dir> moves to the window in that direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2)))

; Handy ways to clean up whitespace around point
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "M-\\") 'delete-horizontal-space)

; Finding files
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

; Always auto-indent on a newline. This will probably have to be limited.
(global-set-key (kbd "<return>") 'newline-and-indent)

; Jumping from spec to implementation and back
(add-hook 'ruby-mode-hook
	  (lambda ()
	    (define-key ruby-mode-map (kbd "C-c j s") 'jump-to-spec-file)
	    (define-key ruby-mode-map (kbd "C-c j i") 'jump-to-implementation-file)))

(provide 'my-key-bindings)
