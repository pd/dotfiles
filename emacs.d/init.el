;; -*- mode: Emacs-Lisp -*-

; Dir names useful everywhere else
(setq user-vendor-emacs-directory  (expand-file-name "vendor/" user-emacs-directory)
      user-private-emacs-directory (expand-file-name "../private/emacs.d/" user-emacs-directory)
      source-directory (expand-file-name "~/vendor/emacs-24.2"))

; Core load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path user-vendor-emacs-directory)
(add-to-list 'load-path user-private-emacs-directory)

; Boot.
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

; Libraries some day.
(require 'pd/smart-shell)
(require 'pd/zsh-dir-aliases)
(require 'pd/emacs-inspection)
(add-to-list 'load-path (expand-file-name "~/sauce/quickref.el/"))
(require 'quickref)

; Boot a server, in case I somehow ended up without one.
; This allows emacsclient to seamlessly work everywhere.
(require 'server)
(unless server-process
  (server-start))

; Private parts.
(when (file-exists-p (expand-file-name "init.el" user-private-emacs-directory))
  (load (expand-file-name "init.el" user-private-emacs-directory)))

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
