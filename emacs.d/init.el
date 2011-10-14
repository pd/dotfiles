;; -*- mode: Emacs-Lisp -*-

(add-to-list 'load-path "~/.emacs.d")

(require 'pd/defuns)
(require 'pd/core)
(require 'pd/magit)
(require 'pd/coding)
(require 'pd/bindings)
(require 'pd/ido)
(require 'pd/packages)
(require 'pd/org)

; focused
(require 'pd/shell)
(require 'pd/tramp)
(require 'pd/p4)
(require 'pd/lisps)
(require 'pd/ruby)
(require 'pd/js)
(require 'pd/haskell)
(require 'pd/scala)
(require 'pd/irc)
(require 'pd/smex)

; wasn't it silly to have pd/misc in the "focused" section?
; this is an artificial comment to put an end to such madness.
(require 'pd/misc)

; private
(when (pd/has-private-emacsd-p)
  (pd/load-private-emacsd))

; boot the emacs server if it's not available yet
(require 'server)
(unless (or (server-running-p) (daemonp))
  (server-start))

(turn-on-save-visited-files-mode)
