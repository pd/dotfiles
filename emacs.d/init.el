;; -*- mode: Emacs-Lisp -*-

(add-to-list 'load-path "~/.emacs.d")

; generic
(require 'pd/core)
(require 'pd/packages)
(require 'pd/defuns)
(require 'pd/bindings)
(require 'pd/ido)
(require 'pd/theme)
(require 'pd/coding)

; focused
(require 'pd/shell)
(require 'pd/lisps)
(require 'pd/ruby)
(require 'pd/js)
(require 'pd/haskell)
(require 'pd/haml-sass)
(require 'pd/magit)
(require 'pd/irc)
(require 'pd/chrome)
(require 'pd/smex)
