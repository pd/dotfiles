;; -*- mode: Emacs-Lisp -*-

(add-to-list 'load-path "~/.emacs.d")

(require 'pd/defuns)
(require 'pd/core)
(when (pd/macosx-p)
  (require 'pd/osx))

(require 'pd/coding)
(require 'pd/bindings)
(require 'pd/ido)

; boot ELPA
(require 'package)
(package-initialize)

; (require 'pd/org)
; 
; (require 'pd/shell)
; (require 'pd/tramp)
; (require 'pd/lisps)
; (require 'pd/ruby)
; (require 'pd/js)
; (require 'pd/irc)
; (require 'pd/smex)
; (require 'pd/pcmpl-powify)
; 
; (require 'pd/misc)

; old crap
; (require 'pd/p4)
; (require 'pd/haskell)
; (require 'pd/scala)

; private
; (when (pd/has-private-emacsd-p)
;  (pd/load-private-emacsd))

; boot the emacs server if it's not available yet
; (require 'server)
; (unless (or (server-running-p) (daemonp))
;   (server-start))
; 
; (turn-on-save-visited-files-mode)
