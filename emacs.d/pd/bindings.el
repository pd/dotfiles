; i hit the menu key aiming for left arrow all the time
(global-unset-key (kbd "<menu>"))

; remove a few unnecessary text movement aliases,
; some habits i want to break
(global-unset-key (kbd "M-<left>"))
(global-unset-key (kbd "M-<right>"))
(global-unset-key (kbd "M-}"))
(global-unset-key (kbd "M-{"))

; text navigation
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

; C-x C-b: ibuffer
; C-c C-b ...: misc buffer ops
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-b b") 'bury-buffer)
(global-set-key (kbd "C-c C-b r") 'rename-buffer)

; super-[left/up/down/right]: window navigation
(windmove-default-keybindings 'super)

; M-S-[left/up/down/right]: window resizing
; the directions unfortunately don't always make sense in context
(global-set-key (kbd "M-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-S-<up>") 'enlarge-window)
(global-set-key (kbd "M-S-<down>") 'shrink-window)
(global-set-key (kbd "M-S-<right>") 'enlarge-window-horizontally)

; recent files
(global-set-key (kbd "C-x f") 'pd/recentf-ido-find-file)

; text editing
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "M-<return>") 'pd/append-and-move-to-new-line)
(global-set-key (kbd "M-S-<return>") 'pd/prepend-and-move-to-new-line)
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)

; misc
(global-set-key (kbd "C-c s") 'shell)
(global-set-key (kbd "<f6>") 'linum-mode)
(global-set-key (kbd "C-h a") 'apropos) ; defaults to command-apropos
(global-set-key (kbd "C-c m") 'woman)

; mac fullscreening
(when (pd/macosx-p)
  (global-set-key (kbd "s-S-<return>") 'ns-toggle-fullscreen))

(provide 'pd/bindings)
