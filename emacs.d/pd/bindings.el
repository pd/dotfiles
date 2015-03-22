(require 'pd/kill-tracker)
(require 'bind-key)
(require 'discover)

;; M-x
(bind-keys*
 ("M-x" . smex)
 ("M-X" . smex-major-mode-commands))

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
