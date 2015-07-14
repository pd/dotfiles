(require 'evil)
(require 'evil-leader)

(setq evil-leader/leader "<SPC>")
(global-evil-leader-mode +1)
(evil-mode +1)

(evil-leader/set-key
  "bb" 'pd/message-file-name
  "br" 'rename-buffer
  "bR" 'pd/rename-buffer-and-file
  "bz" 'pd/reload-buffer
  "bZ" 'revert-buffer
  "bp" 'previous-buffer
  "bn" 'next-buffer)

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "eb" 'eval-buffer
  "ed" 'eval-defun
  "er" 'eval-region
  "es" 'eval-last-sexp
  "eS" 'eval-print-last-sexp)

(evil-leader/set-key
  "jb" 'bookmark-jump
  "jB" 'bookmark-set
  "jf" 'find-function
  "ji" 'pd/find-init.el
  "jl" 'find-library
  "jk" 'find-function-on-key
  "jv" 'find-variable)

(evil-leader/set-key
  "tc" 'transpose-chars
  "tw" 'transpose-words
  "tl" 'transpose-lines
  "ts" 'transpose-sexps)

(evil-leader/set-key
  "x'" 'pd/shell-switcher-switch-or-new-buffer
  "xd" 'edbi:open-db-viewer
  "xe" 'ielm
  "xj" 'nodejs-repl
  "xr" 'run-ruby
  "xx" 'rtog/toggle-repl)

(provide 'pd/evil)
