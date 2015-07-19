(require 'pd/kill-tracker)
(require 'bind-key)

;; M-x
(bind-keys*
 ("M-x" . smex)
 ("M-X" . smex-major-mode-commands))

;; buffers
(bind-key "C-x C-b" 'ibuffer)

(bind-keys*
 ("C-c b n" . pd/message-file-name)
 ("C-c b r" . rename-buffer)
 ("C-c b R" . pd/rename-buffer-and-file)
 ("C-c b z" . pd/reload-buffer)
 ("C-c b Z" . revert-buffer)
 ("C-c b b" . previous-buffer)
 ("C-c b f" . next-buffer))

;; windows
(bind-keys*
 ("s-h" . windmove-left)
 ("s-j" . windmove-down)
 ("s-k" . windmove-up)
 ("s-l" . windmove-right))

;; frames
(bind-keys*
 ("M-C-+" . zoom-frm-in)
 ("M-C-_" . zoom-frm-out)
 ("M-C--" . zoom-frm-out))

;; files
(bind-keys*
 ("C-c s"   . ag)
 ("C-x C-d" . dired)
 ("C-c f f" . ffap)
 ("C-c f p" . find-file-in-repository)
 ("C-c f l" . pd/kill-tracker-find-last))

;; text navigation
(bind-keys*
 ("C-a"   . smarter-move-beginning-of-line)
 ("C-="   . er/expand-region)
 ("M-i"   . imenu)
 ("M-C-[" . backward-paragraph)
 ("M-C-]" . forward-paragraph))

;; text editing
(bind-keys*
 ("M-/"        . hippie-expand)
 ("M-;"        . pd/comment-dwim)
 ("C-S-k"      . kill-whole-line)
 ("C-c w"      . delete-trailing-whitespace)
 ("C-\\"       . delete-horizontal-space)
 ("C-c / s"    . replace-string)
 ("C-c / r"    . replace-regexp)
 ("C-c ="      . align-regexp)
 ("C-^"        . pd/join-next-line)
 ("M-RET"      . pd/open-line-before)
 ("<C-return>" . pd/open-line-after))

;; multiple-cursors
(bind-keys*
 ("H-SPC"     . set-rectangular-region-anchor)
 ("C-c m SPC" . set-rectangular-region-anchor)
 ("C->"       . mc/mark-next-like-this)
 ("C-<"       . mc/mark-previous-like-this)
 ("C-%"       . mc/mark-all-like-this)
 ("C-c m e"   . mc/edit-lines)
 ("C-c m C-a" . mc/edit-beginnings-of-lines)
 ("C-c m C-e" . mc/edit-ends-of-lines)
 ("C-c m y"   . yank-rectangle)) ; C-x r y, but i think of it in terms of mc/...

;; M-t: transpositions
(unbind-key "M-t")
(bind-keys*
 ("M-t c" . transpose-chars)
 ("M-t w" . transpose-words)
 ("M-t l" . transpose-lines)
 ("M-t s" . transpose-sexps))

;; C-c j: jump
(bind-keys*
 ("C-c j b" . bookmark-jump)
 ("C-c j B" . bookmark-set)
 ("C-c j f" . find-function)
 ("C-c j i" . pd/find-init.el)
 ("C-c j l" . find-library)
 ("C-c j k" . find-function-on-key)
 ("C-c j v" . find-variable))

;; C-c x: repls
(bind-keys*
 ("C-c x '" . pd/shell-switcher-switch-or-new-buffer)
 ("C-c x d" . edbi:open-db-viewer)
 ("C-c x e" . ielm)
 ("C-c x j" . nodejs-repl)
 ("C-c x r" . run-ruby)
 ("C-c x x" . rtog/toggle-repl))

;; misc
(bind-key "C-x g" 'magit-status)
(bind-key "C-h a" 'apropos)

;; rm -rf annoyances
(unbind-key "<menu>")
(unbind-key "C-x C-z")
(unbind-key "C-z")
(unbind-key "C-x m")

(provide 'pd/bindings)
