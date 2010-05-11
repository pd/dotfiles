; i hit the menu key aiming for left arrow all the time;
; and hit M-` while trying to move around in xmonad
(global-unset-key (kbd "<menu>"))
(global-unset-key (kbd "M-`"))

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
(global-set-key (kbd "C-c b b") 'bury-buffer)
(global-set-key (kbd "C-c b r") 'rename-buffer)

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
(global-set-key (kbd "C-c C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c r") 'revert-buffer)

; C-c x: launch various repls
(global-set-key (kbd "C-c x e") 'ielm)
(global-set-key (kbd "C-c x r") 'run-ruby)
(global-set-key (kbd "C-c x p") 'run-python)
(global-set-key (kbd "C-c x h") 'run-haskell)

; misc
(global-set-key (kbd "C-c s") 'shell)
(global-set-key (kbd "<f6>") 'linum-mode)
(global-set-key (kbd "C-h a") 'apropos) ; defaults to command-apropos
(global-set-key (kbd "C-c m") 'woman)
(global-set-key (kbd "C-c y") 'x-clipboard-yank)

; mac fullscreening
(when (pd/macosx-p)
  (global-set-key (kbd "s-S-<return>") 'ns-toggle-fullscreen))

(provide 'pd/bindings)
