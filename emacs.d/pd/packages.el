; boot elpa
(require 'find-func)
(when (or (find-library-name "package")
          (load (expand-file-name "~/.emacs.d/elpa/package.el")))
  (package-initialize))

; emacs' built-in elpa only uses the GNU repo. lame.
(dolist (source '(("technomancy" . "http://repo.technomancy.us/emacs/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))

; but elpa doesn't have it all
(labels ((add-path (p)
                   (add-to-list 'load-path p)))
  (add-path "~/.emacs.d/vendor")
  (add-path "~/.emacs.d/vendor/ensime/elisp")
  (add-path "~/dotfiles/vendor/magit")
  (add-path "~/dotfiles/vendor/smex")
  (add-path "~/dotfiles/vendor/inf-ruby-bond")
  (add-path "~/dotfiles/vendor/erc")
  (add-path "~/dotfiles/vendor/scala-mode")
  (add-path "~/dotfiles/vendor/p4el")
  (add-path "~/dotfiles/vendor/workgroups")
  (add-path "~/dotfiles/vendor/cucumber.el"))

(provide 'pd/packages)
