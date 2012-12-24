;; -*- mode: Emacs-Lisp -*-

(add-to-list 'load-path user-emacs-directory)

; I grabbed ruby-mode from emacs HEAD, as I don't really feel like waiting
; for that to build. This should be in 24.3 when it's released.
(add-to-list 'load-path (expand-file-name "vendor/" user-emacs-directory))

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
(require 'pcmpl-git) ; from a local melpa package build atm. TODO: get it into ELPA

; Core.
(require 'pd/defuns)
(require 'pd/core)
(when (pd/macosx-p)
  (require 'pd/osx))
(require 'pd/auto-mode)
(require 'pd/coding)
(require 'pd/bindings)
(require 'pd/mode-line)
(require 'pd/theme)
(require 'pd/ido)

; Everything else.
(require 'pd/lisps)
(require 'pd/ruby)
(require 'pd/pcmpl-powify)
(require 'pd/misc)

(require 'server)
(unless server-process
  (server-start))

; Private parts.
(when (pd/has-private-emacsd-p)
  (pd/load-private-emacsd))

; Emacs C source location
(when (file-exists-p (expand-file-name "~/vendor/emacs-24.2"))
  (setq source-directory (expand-file-name "~/vendor/emacs-24.2")))

;; .emacs.d/after-loads/*.el
(require 'easy-after-load)
(easy-after-load)

(require 'smex)
