;; -*- mode: Emacs-Lisp -*-

(add-to-list 'load-path user-emacs-directory)

; I grabbed ruby-mode from emacs HEAD, as I don't really feel like waiting
; for that to build. This should be in 24.3 when it's released.
(add-to-list 'load-path (expand-file-name "vendor/" user-emacs-directory))

; Immediately load the basics.
(require 'pd/core)

; ELPA.
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

; Packages I always want.
(require 'cl)
(require 'ido)
(require 'uniquify)
(require 'winner)
(require 's)
(require 'magit)
(require 'rvm)
(require 'buffer-move)
(require 'ack-and-a-half)

; Mine.
(require 'pd/defuns)
(when (pd/macosx-p)
  (require 'pd/osx))
(require 'pd/coding)
(require 'pd/bindings)
(require 'pd/mode-line)
(require 'pd/theme)
(require 'pd/ido)
(require 'pd/lisps)
(require 'pd/misc)

; Boot a server, in case I somehow ended up without one.
; This allows emacsclient to seamlessly work everywhere.
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
(easy-after-load)

;; Easier mode detection
(easy-auto-mode
  '((ruby-mode "\\.rake$" "Rakefile$" "Guardfile$" "Gemfile$"
               "\\.gemspec$" "\\.?irbrc$" "\\.rabl$" "\\.ru$")
    (markdown-mode "\\.md$" "\\.markdown$")))

(require 'smex)
