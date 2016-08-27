(require 'pd/kill-tracker)
(require 'bind-key)

(setq helm-command-prefix-key "C-c h")

;; M-x
(bind-keys*
 ("M-x" . helm-M-x))

;; buffers
(bind-key "C-x b"   'helm-mini)
(bind-key "C-x C-b" 'helm-buffers-list)

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
 ("M-C-0" . zoom-frm-unzoom)
 ("M-C-)" . zoom-frm-unzoom) ; aka M-C-S-0
 ("M-C-+" . zoom-frm-in)
 ("M-C-_" . zoom-frm-out)
 ("M-C--" . zoom-frm-out))

;; files
(bind-keys*
 ("C-x C-d" . dired)
 ("C-c f f" . find-file-at-point)
 ("C-c f ." . find-file-at-point)
 ("H-l"     . pd/kill-tracker-find-last))

;; projectile
(bind-keys*
 ("C-c p a" . helm-projectile-ag)
 ("C-c p b" . helm-projectile-switch-to-buffer)
 ("C-c p f" . helm-projectile-find-file)
 ("C-c p p" . helm-projectile-switch-project))

;; find via helm, sans projectile
(bind-key "s-a" 'helm-ag)
(bind-key "s-g" 'helm-rubygems-local)
(bind-key "s-p" 'helm-projectile-switch-project)
(bind-key "M-i" 'helm-imenu)
(bind-key "s-y" 'helm-show-kill-ring)

;; text navigation
(bind-keys*
 ("C-a"   . smarter-move-beginning-of-line)
 ("C-="   . er/expand-region)
 ("M-C-[" . backward-paragraph)
 ("M-C-]" . forward-paragraph))

;; text editing
(bind-keys*
 ("M-/"        . hippie-expand)
 ("M-;"        . pd/comment-dwim)
 ("C-:"        . helm-command)
 ("C-S-k"      . kill-whole-line)
 ("C-c w"      . delete-trailing-whitespace)
 ("C-\\"       . delete-horizontal-space)
 ("C-c / s"    . replace-string)
 ("C-c / r"    . replace-regexp)
 ("C-c ="      . align-regexp)
 ("C-^"        . pd/join-next-line)
 ("M-RET"      . pd/open-line-before)
 ("<C-return>" . pd/open-line-after))

;; Hyper: multiple-cursors
(bind-keys*
 ("H-SPC" . set-rectangular-region-anchor)
 ("H->"   . mc/mark-next-like-this)
 ("H-<"   . mc/mark-previous-like-this)
 ("H-g"   . mc/mark-all-like-this)
 ("H-."   . mc/edit-lines)
 ("H-a"   . mc/edit-beginnings-of-lines)
 ("H-e"   . mc/edit-ends-of-lines)
 ("H-y"   . yank-rectangle)) ; C-x r y, but i think of it in terms of mc/...

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
 ("C-M-\""  . helm-mt)
 ("C-c x d" . edbi:open-db-viewer)
 ("C-c x e" . ielm)
 ("C-c x j" . nodejs-repl)
 ("C-c x r" . run-ruby)
 ("C-c x x" . rtog/toggle-repl))

;; find bindings
(bind-keys*
 ("C-h C-b" . discover-my-major)
 ("C-h M-b" . discover-my-mode))

;; better help
(bind-keys*
 ("C-h a" . helm-apropos)
 ("C-h d" . dash-at-point)
 ("C-h m" . helm-describe-modes))

;; misc
(bind-key "C-x g" 'magit-status)
(bind-key "C-c C-l" 'helm-minibuffer-history minibuffer-local-map)

;; rm -rf annoyances
(unbind-key "<menu>")
(unbind-key "C-x C-z")
(unbind-key "C-z")
(unbind-key "C-x m")

(provide 'pd/bindings)
