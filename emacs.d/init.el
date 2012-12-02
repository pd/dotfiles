;; -*- mode: Emacs-Lisp -*-

(add-to-list 'load-path "~/.emacs.d")

; Core.
(require 'pd/defuns)
(require 'pd/core)
(when (pd/macosx-p)
  (require 'pd/osx))

(require 'pd/coding)
(require 'pd/bindings)
(require 'pd/ido)

; ELPA.
(require 'package)
(package-initialize)

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

(require 'pd/smex)
