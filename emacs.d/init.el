;; -*- mode: Emacs-Lisp -*-

(add-to-list 'load-path user-emacs-directory)

; ELPA.
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

; Packages I will pretty much always want.
(require 'cl)
(require 's)
(require 'magit)
(require 'rvm)
(require 'buffer-move)
(require 'ack-and-a-half)

; Core.
(require 'pd/defuns)
(require 'pd/core)
(when (pd/macosx-p)
  (require 'pd/osx))
(require 'pd/auto-mode)
(require 'pd/coding)
(require 'pd/bindings)
(require 'pd/theme)
(require 'pd/ido)

; Everything else.
(require 'pd/org)
(require 'pd/shell)
(require 'pd/tramp)
(require 'pd/lisps)
(require 'pd/ruby)
(require 'pd/js)
(require 'pd/pcmpl-powify)
(require 'pd/misc)

(require 'server)
(unless server-process
  (server-start))

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
