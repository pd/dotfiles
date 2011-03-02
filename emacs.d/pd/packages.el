; boot elpa
(require 'find-func)
(when (or (find-library-name "package")
          (load (expand-file-name "~/.emacs.d/elpa/package.el")))
  (package-initialize))

; but elpa doesn't have it all
(labels ((add-path (p)
                   (add-to-list 'load-path p)))
  (add-path "~/.emacs.d/vendor")
  (add-path "~/dotfiles/vendor/magit")
  (add-path "~/dotfiles/vendor/smex")
  (add-path "~/dotfiles/vendor/emacs_chrome/servers")
  (add-path "~/dotfiles/vendor/inf-ruby-bond"))

(provide 'pd/packages)
