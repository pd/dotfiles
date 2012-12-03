(defun global-set-keys (bindings)
  (dolist (binding bindings)
    (let ((key (car binding)) (command (cadr binding)))
      (global-set-key (read-kbd-macro key) command))))

(defun global-unset-keys (keys)
  (dolist (key keys)
    (global-unset-key (read-kbd-macro key))))

(global-unset-keys
 '("<menu>"

   ; Habits I want to break
   "M-<left>" "M-<right>" "M-}" "M-{"

   ; suspend-frame is annoying
   "C-x C-z" "C-z"

   ; no, i don't want to compose an email
   "C-x m"))

(global-set-keys
 '(; text navigation
   ("M-p" backward-paragraph)
   ("M-n" forward-paragraph)
   ("C-a" pd/back-to-indentation-or-beginning-of-line)
   ("C-`" 'pd/push-mark)
   ("M-`" 'pd/jump-to-mark)

   ; C-x C-b: ibuffer
   ; C-c C-b ...: misc buffer ops
   ("C-x C-b" ibuffer)
   ("C-c b b" bury-buffer)
   ("C-c b r" rename-buffer)
   ("C-c b z" reload-buffer)

   ; super-[left/up/down/right]: window navigation
   ; super-[hjkl]: even better.
   ("s-h" windmove-left)
   ("s-j" windmove-down)
   ("s-k" windmove-up)
   ("s-l" windmove-right)

   ; M-S-[left/up/down/right]: window resizing
   ; the directions unfortunately don't always make sense in context
   ("M-S-<left>" shrink-window-horizontally)
   ("M-S-<up>" enlarge-window)
   ("M-S-<down>" shrink-window)
   ("M-S-<right>" enlarge-window-horizontally)

   ; find files
   ("C-x M-f" ffap)
   ("C-x C-d" dired) ; i never, ever want list-directory

   ; text editing
   ("M-/" hippie-expand)
   ("C-S-k" kill-whole-line)
   ("C-c w" delete-trailing-whitespace)
   ("M-<return>" pd/append-and-move-to-new-line)
   ("M-S-<return>" pd/prepend-and-move-to-new-line)
   ("C-c #" comment-or-uncomment-region)
   ("C-c r" revert-buffer)
   ("C-c / s" replace-string)
   ("C-c / r" replace-regexp)
   ("C-c =" align-regexp)

   ; C-c x: launch various repls
   ("C-c x e" ielm)
   ("C-c x l" slime) ; how this defaults to clojure i dunno
   ("C-c x r" run-ruby)
   ("C-c x p" run-python)
   ("C-c x h" run-haskell)
   ("C-c x j" run-js)
   ("C-c x m" run-mozilla)

   ; misc
   ("s-+" pd/increase-font-size)
   ("s-_" pd/decrease-font-size)
   ("C-c f" ffap)
   ("C-c s" pd/smart-shell)
   ("C-c M-s" pd/ido-switch-shell)
   ("C-c t" pd/term)
   ("<f6>" linum-mode)
   ("C-c m" man)
   ("C-c y" x-clipboard-yank)
   ("C-h a" apropos) ; defaults to command-apropos

   ))

; Also add the default super-{left,right,up,down} bindings
(windmove-default-keybindings 'super)

; mac only
(when (pd/macosx-p)
  (global-set-key (kbd "s-S-<return>") 'ns-toggle-fullscreen)
  (if (eq ns-alternate-modifier 'meta)
      (global-unset-key (kbd "M-p")) ; stop asking me to print please
    (global-unset-key (kbd "s-p"))))

(provide 'pd/bindings)
