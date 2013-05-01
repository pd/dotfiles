;; -*- mode: Emacs-Lisp -*-

; Dir names useful everywhere else
(setq user-vendor-emacs-directory  (expand-file-name "vendor/" user-emacs-directory)
      user-private-emacs-directory (expand-file-name "~/dotfiles/private/emacs.d/")
      source-directory (expand-file-name "~/vendor/emacs-24.2"))

; Core load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path user-vendor-emacs-directory)
(add-to-list 'load-path user-private-emacs-directory)

; Boot.
(require 'pd/benchmark)
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
(require 'smex)
(require 'uniquify)
(require 'winner)
(require 'keydef)
(require 'smart-tab)
(require 'quickref)
(require 'ack-and-a-half)
(require 'auto-complete)
(require 'shell-switcher)

; Mine.
(require 'pd/defuns)
(require 'pd/coding)
(require 'pd/bindings)
(require 'pd/mode-line)
(require 'pd/theme)
(require 'pd/lisps)
(require 'pd/melpa)
(require 'pd/misc)
(when (pd/macosx-p)
  (require 'pd/osx))

; Libraries some day.
(require 'pd/zsh-dir-aliases)
(require 'pd/emacs-inspection)

; Boot a server, in case I somehow ended up without one.
; This allows emacsclient to seamlessly work everywhere.
(require 'server)
(unless server-process
  (server-start))

; Private parts.
(when (file-exists-p (expand-file-name "init.el" user-private-emacs-directory))
  (load (expand-file-name "init.el" user-private-emacs-directory)))

; Emacs C source location
(let ((brew-source (expand-file-name "~/Library/Caches/Homebrew/emacs--git")))
  (when (file-exists-p brew-source)
    (setq source-directory brew-source)))

;; Some auto-saved configuration files should live in ~/.emacs.d/store
(let ((store (expand-file-name "store/" user-emacs-directory)))
  (setq mc/list-file       (expand-file-name "mc-lists.el" store)
        quickref-save-file (expand-file-name "quickrefs.el" store)))

;; .emacs.d/after-loads/*.el
(easy-after-load)

;; Easier mode detection
(easy-auto-mode
  '((ruby-mode "\\.rake\\'" "Rakefile\\'" "Guardfile\\'" "Gemfile\\'"
               "\\.gemspec\\'" "\\.?irbrc\\'" "\\.rabl\\'" "\\.ru\\'"
               "\\.simplecov\\'" "\\.erb\\'")
    (js-mode "\\.json\\'")
    (js2-mode "\\.js\\'")
    (markdown-mode "\\.md\\'" "\\.markdown\\'")
    (gitconfig-mode "gitconfig\\'")
    (gitignore-mode "gitignore\\'")))

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
