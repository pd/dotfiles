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
(require 'pd/yasnippet)

; focused
(require 'pd/shell)
(require 'pd/tramp)
(require 'pd/magit)
(require 'pd/lisps)
(require 'pd/ruby)
(require 'pd/js)
(require 'pd/haskell)
(require 'pd/scala)
(require 'pd/yaml)
(require 'pd/irc)
(require 'pd/misc)
(require 'pd/smex)
