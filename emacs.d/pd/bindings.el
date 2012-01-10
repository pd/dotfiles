; i hit the menu key aiming for left arrow all the time;
; and hit M-` while trying to move around in xmonad
(global-unset-key (kbd "<menu>"))

; remove a few unnecessary text movement aliases,
; some habits i want to break
(global-unset-key (kbd "M-<left>"))
(global-unset-key (kbd "M-<right>"))
(global-unset-key (kbd "M-}"))
(global-unset-key (kbd "M-{"))

; suspend-frame is terribly annoying
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))

; i don't want to compose emails
(global-unset-key (kbd "C-x m"))

; text navigation
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "C-`") 'pd/push-mark)
(global-set-key (kbd "M-`") 'pd/jump-to-mark)

; C-x C-b: ibuffer
; C-c C-b ...: misc buffer ops
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c b b") 'bury-buffer)
(global-set-key (kbd "C-c b r") 'rename-buffer)
(global-set-key (kbd "C-c b z") 'reload-buffer)

; super-[left/up/down/right]: window navigation
; super-[hjkl]: even better.
(windmove-default-keybindings 'super)
(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-l") 'windmove-right)

; M-S-[left/up/down/right]: window resizing
; the directions unfortunately don't always make sense in context
(global-set-key (kbd "M-S-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "M-S-<up>")    'enlarge-window)
(global-set-key (kbd "M-S-<down>")  'shrink-window)
(global-set-key (kbd "M-S-<right>") 'enlarge-window-horizontally)

; find files
(global-set-key (kbd "C-x M-f") 'ffap)
(global-set-key (kbd "C-x f")   'pd/recentf-ido-find-file)
(global-set-key (kbd "C-x C-d") 'dired) ; i never, ever want list-directory

; text editing
(global-set-key (kbd "M-/")          'hippie-expand)
(global-set-key (kbd "C-S-k")        'kill-whole-line)
(global-set-key (kbd "C-c w")        'delete-trailing-whitespace)
(global-set-key (kbd "M-<return>")   'pd/append-and-move-to-new-line)
(global-set-key (kbd "M-S-<return>") 'pd/prepend-and-move-to-new-line)
(global-set-key (kbd "C-c C-;")      'comment-or-uncomment-region)
(global-set-key (kbd "C-c r")        'revert-buffer)
(global-set-key (kbd "C-c C-s")      'replace-string)
(global-set-key (kbd "C-c M-s")      'replace-regexp)
(global-set-key (kbd "C-c =")        'align-regexp)

; C-c x: launch various repls
(global-set-key (kbd "C-c x e") 'ielm)
(global-set-key (kbd "C-c x l") 'slime) ; how this defaults to clojure i dunno
(global-set-key (kbd "C-c x r") 'run-ruby)
(global-set-key (kbd "C-c x p") 'run-python)
(global-set-key (kbd "C-c x h") 'run-haskell)
(global-set-key (kbd "C-c x j") 'run-js)
(global-set-key (kbd "C-c x m") 'run-mozilla)

; misc
(global-set-key (kbd "M-+")   'pd/increase-font-size)
(global-set-key (kbd "M-_")   'pd/decrease-font-size)
(global-set-key (kbd "C-c f") 'ffap)
(global-set-key (kbd "C-c s") 'shell)
(global-set-key (kbd "C-c t") 'pd/term)
(global-set-key (kbd "<f6>")  'linum-mode)
(global-set-key (kbd "C-c m") 'woman)
(global-set-key (kbd "C-c y") 'x-clipboard-yank)
(global-set-key (kbd "C-h a") 'apropos) ; defaults to command-apropos

; mac only
(when (pd/macosx-p)
  (global-set-key (kbd "s-S-<return>") 'ns-toggle-fullscreen)
  (if (eq ns-alternate-modifier 'meta)
      (global-unset-key (kbd "M-p")) ; stop asking me to print please
    (global-unset-key (kbd "s-p"))))

(provide 'pd/bindings)
