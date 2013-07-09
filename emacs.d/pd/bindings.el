(require 'bind-key)
(require 'pd/kill-tracker)

;; M-x
(bind-key "M-x" 'smex)
(bind-key "M-X" 'smex-major-mode-commands)

;; buffers
(bind-key "C-x C-b" 'ibuffer)
(bind-key "C-c b n" 'pd/message-file-name)
(bind-key "C-c b r" 'rename-buffer)
(bind-key "C-c b R" 'pd/rename-buffer-and-file)
(bind-key "C-c b z" 'pd/reload-buffer) ; rerun hooks
(bind-key "C-c b Z" 'revert-buffer)    ; actually reload the file
(bind-key "C-c b b" 'previous-buffer)
(bind-key "C-c b f" 'next-buffer)

;; windows
(bind-key "s-h" 'windmove-left)
(bind-key "s-j" 'windmove-down)
(bind-key "s-k" 'windmove-up)
(bind-key "s-l" 'windmove-right)

;; frames
(bind-key "M-C-+" 'zoom-frm-in)
(bind-key "M-C-_" 'zoom-frm-out)
(bind-key "M-C--" 'zoom-frm-out)

;; files
(bind-key "C-c s"   'ag)
(bind-key "C-x C-d" 'dired)
(bind-key "C-c f f" 'ffap)
(bind-key "C-c f p" 'find-file-in-repository)
(bind-key "C-c f l" 'pd/kill-tracker-find-last)

;; text navigation
(bind-key "C-a"   'smarter-move-beginning-of-line)
(bind-key "C-="   'er/expand-region)
(bind-key "M-i"   'imenu)
(bind-key "M-C-[" 'backward-paragraph)
(bind-key "M-C-]" 'forward-paragraph)

;; text editing
(bind-key "M-/"        'hippie-expand)
(bind-key "M-;"        'pd/comment-dwim)
(bind-key "C-S-k"      'kill-whole-line)
(bind-key "C-c w"      'delete-trailing-whitespace)
(bind-key "C-\\"       'delete-horizontal-space)
(bind-key "C-c / s"    'replace-string)
(bind-key "C-c / r"    'replace-regexp)
(bind-key "C-c ="      'align-regexp)
(bind-key "C-^"        'pd/join-next-line)
(bind-key "M-RET"      'pd/open-line-before)
(bind-key "<C-return>" 'pd/open-line-after)

;; multiple-cursors
(bind-key "H-SPC"     'set-rectangular-region-anchor)
(bind-key "C-c m SPC" 'set-rectangular-region-anchor)
(bind-key "C->"       'mc/mark-next-like-this)
(bind-key "C-<"       'mc/mark-previous-like-this)
(bind-key "C-%"       'mc/mark-all-like-this)
(bind-key "C-c m e"   'mc/edit-lines)
(bind-key "C-c m C-a" 'mc/edit-beginnings-of-lines)
(bind-key "C-c m C-e" 'mc/edit-ends-of-lines)
(bind-key "C-c m y"   'yank-rectangle) ; C-x r y, but i think of it in terms of mc/...

;; M-t: transpositions
(unbind-key "M-t")
(bind-key "M-t c" 'transpose-chars)
(bind-key "M-t w" 'transpose-words)
(bind-key "M-t l" 'transpose-lines)
(bind-key "M-t s" 'transpose-sexps)

;; C-c j: jump
(bind-key "C-c j b"   'bookmark-jump)
(bind-key "C-c j f"   'find-function)
(bind-key "C-c j i"   'pd/find-init.el)
(bind-key "C-c j l"   'find-library)
(bind-key "C-c j k"   'find-function-on-key)
(bind-key "C-c j v"   'find-variable)
(bind-key "C-c C-j b" 'bookmark-set)

;; C-c x: repls
(bind-key "C-c x '" 'shell-switcher-switch-buffer)
(bind-key "C-c x e" 'ielm)
(bind-key "C-c x j" 'nodejs-repl)
(bind-key "C-c x r" 'run-ruby)

;; misc
(bind-key "C-x g" 'magit-status)
(bind-key "C-h a" 'apropos)

;; rm -rf annoyances
(unbind-key "<menu>")
(unbind-key "C-x C-z")
(unbind-key "C-z")
(unbind-key "C-x m")

(provide 'pd/bindings)
