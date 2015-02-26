(require 'pd/kill-tracker)
(require 'bind-key)
(require 'discover)

;; M-x
(bind-key "M-x" 'smex)
(bind-key "M-X" 'smex-major-mode-commands)

;; helm?
;(bind-key "M-x" 'helm-M-x)
;(bind-key "C-x b" 'helm-mini)

;; buffers
(bind-key "C-x C-b" 'ibuffer)
(discover-add-context-menu
 :context-menu '(buffers
                 (description "C-c b: buffer manipulation")
                 (actions
                  ("Buffers"
                   ("n" "kill+show file name" pd/message-file-name)
                   ("r" "rename buffer" rename-buffer)
                   ("R" "rename file" pd/rename-buffer-and-file)
                   ("z" "rerun hooks" pd/reload-buffer)
                   ("Z" "reload file" revert-buffer)
                   ("b" "previous buffer" previous-buffer)
                   ("f" "next buffer" next-buffer))))
 :bind "C-c b")

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
(discover-add-context-menu
 :context-menu '(jumps
                 (description "C-c j: jump to something")
                 (actions
                  ("Jump"
                   ("b" "bookmark" bookmark-jump)
                   ("B" "bookmark-set" bookmark-set)
                   ("f" "elisp function" find-function)
                   ("i" "init.el" pd/find-init.el)
                   ("l" "elisp library" find-library)
                   ("k" "elisp function-on-key" find-function-on-key)
                   ("v" "elisp variable" find-variable))))
 :bind "C-c j")

;; C-c x: repls
(discover-add-context-menu
 :context-menu '(repls
                 (description "C-c x: repls")
                 (actions
                  ("REPLs"
                   ("'" "shell-mode" pd/shell-switcher-switch-or-new-buffer)
                   ("d" "edbi" edbi:open-db-viewer)
                   ("e" "ielm" ielm)
                   ("j" "js" nodejs-repl)
                   ("r" "ruby" run-ruby)
                   ("x" "repl-toggle" rtog/toggle-repl))))
 :bind "C-c x")

;; misc
(bind-key "C-x g" 'magit-status)
(bind-key "C-h a" 'apropos)

;; rm -rf annoyances
(unbind-key "<menu>")
(unbind-key "C-x C-z")
(unbind-key "C-z")
(unbind-key "C-x m")

(provide 'pd/bindings)
