;; -*- mode: Emacs-Lisp -*-

;; Globally useful paths
(setq user-vendor-emacs-directory  (expand-file-name "vendor/" user-emacs-directory)
      user-private-emacs-directory (expand-file-name "~/dotfiles/private/emacs.d/"))

(when (file-directory-p (expand-file-name "~/vendor/emacs/src/"))
  (setq source-directory (expand-file-name "~/vendor/emacs/src/")))

(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path user-vendor-emacs-directory)
(add-to-list 'load-path user-private-emacs-directory)

;; Immediately provide 'after macro.
; https://github.com/milkypostman/dotemacs/blob/master/init.el#L57
; TODO move to easy-after-load + autoload, so this is available as
; soon as 'package-initialize is done.
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

;;;; Boot.
(require 'pd/benchmark) ; record time of each require
(require 'pd/bootstrap) ; package.el, turn off noise, favor utf8, etc.
(require 'pd/osx)       ; remap cmd->meta etc as soon as is reasonable
(require 'pd/defuns)    ; generic pd/foo defuns et al
(require 'pd/theme)     ; if anything bombs later, at least emacs'll still be pretty
(require 'pd/bindings)  ; globally interesting bindings
(require 'pd/diminish)  ; after foo-library, diminish.

(require 'pd/core)
(require 'pd/shell)
(require 'pd/prog)
(require 'pd/lisp)
(require 'pd/ruby)
(require 'pd/js)
(require 'pd/markup)

;;;; Some things don't deserve homes.
(after 'dash-at-point
  (add-to-list 'dash-at-point-mode-alist '(js2-mode . "js2")))

(after 'scala-mode2
  (setq scala-indent:indent-value-expression t
        scala-indent:align-forms t
        scala-font-lock:var-face font-lock-variable-name-face))

;;;; Mode detection.
(easy-auto-mode
  '((ruby-mode "\\.rake\\'" "Rakefile\\'" "Guardfile\\'" "Gemfile\\'"
               "\\.gemspec\\'" "\\.?irbrc\\'" "\\.rabl\\'" "\\.ru\\'"
               "\\.simplecov\\'")
    (js-mode "\\.json\\'")
    (js2-mode "\\.js\\'")
    (slim-mode "\\.emblem\\'") ; good enough
    (markdown-mode "\\.md\\'" "\\.markdown\\'")
    (sgml-mode "\\.hbs\\'") ; handlebars-sgml-mode > handlebars-mode
    (gitconfig-mode "gitconfig\\'")
    (gitignore-mode "gitignore\\'")))

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;;;; Start triggering the after-loads.
(require 'uniquify)
(require 'multiple-cursors)
(require 'auto-complete)

;;;; Show me what takes too long.
(save-current-buffer
  (switch-to-buffer "*scratch*" 'norecord)
  (goto-char (point-max))
  (insert (pp (pd/slowest-require-times))))
