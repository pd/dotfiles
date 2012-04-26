(add-to-list 'load-path "~/dotfiles/vendor/espuds")
(require 'espuds)

(add-to-list 'load-path "~/dotfiles/emacs.d/el-get/levenshtein")
(require 'levenshtein)

(add-to-list 'load-path "~/dotfiles/emacs.d")
(require 'pd/defuns)
(require 'pd/shell)
(require 'pd/coding)
(require 'pd/bindings)

(After
 (mapcar (lambda (bufname)
           (let ((process (get-buffer-process bufname)))
             (set-process-query-on-exit-flag process nil)
             (kill-buffer bufname)))
         (pd/shell-buffer-list)))
