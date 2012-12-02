;; -*- mode: Emacs-Lisp -*-

(add-to-list 'load-path "~/.emacs.d")

; ELPA.
(require 'package)
(package-initialize)

; Packages I will pretty much always want.
(require 'cl)
(require 's)
(require 'magit)

; Core.
(require 'pd/defuns)
(require 'pd/core)
(when (pd/macosx-p)
  (require 'pd/osx))

(require 'pd/coding)
(require 'pd/bindings)
(require 'pd/ido)

; Set the theme now that it's loaded.
(load-theme 'molokai t)

; Everything else.
(require 'pd/org)
(require 'pd/shell)
(require 'pd/tramp)
(require 'pd/lisps)
(require 'pd/ruby)
(require 'pd/js)
(require 'pd/pcmpl-powify)
(require 'pd/misc)

; Private parts.
(when (pd/has-private-emacsd-p)
  (pd/load-private-emacsd))

; Add magical eval-after-load for everything in package-inits/*.el
(setq pd/package-init-directory (file-name-as-directory (concat user-emacs-directory "package-inits")))
(dolist (file (directory-files pd/package-init-directory))
  (let ((lib (s-with file (s-chop-prefix "init-") (s-chop-suffix ".el"))))
    (eval-after-load (intern lib)
      `(load ,(concat pd/package-init-directory file)))))

(require 'pd/smex)
