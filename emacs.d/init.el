(setq package-user-dir (expand-file-name (locate-user-emacs-file (concat ".cask/" emacs-version "/elpa"))))
(package-initialize)

;; give in and let custom.el do its thing.
; i still never actually use customize, but some libraries just
; Really Friggin Insist on writing to it themselves. meh.
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; Globally useful paths
(setq user-private-emacs-directory (expand-file-name "~/dotfiles/private/emacs.d/"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path user-private-emacs-directory)

(when (file-directory-p (expand-file-name "~/vendor/emacs/src/"))
  (setq source-directory (expand-file-name "~/vendor/emacs/src/")))

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
(require 'pd/linux)     ; at least get a working $PATH and decent font
(require 'pd/defuns)    ; generic pd/foo defuns et al
(require 'pd/theme)     ; if anything bombs later, at least emacs'll still be pretty
(require 'pd/bindings)  ; globally interesting bindings
(require 'pd/diminish)  ; after foo-library, diminish.

(require 'pd/core)
(require 'pd/shell)
(require 'pd/prog)
(require 'pd/lisp)
(require 'pd/ruby)
(require 'pd/go)
(require 'pd/js)
(require 'pd/markup)
(require 'pd/db)
(require 'pd/irc)

;;;; Some things don't deserve homes.
(after 'dash-at-point
  (add-to-list 'dash-at-point-mode-alist '(js2-mode . "js2")))

(after 'scala-mode2
  (setq scala-indent:indent-value-expression t
        scala-indent:align-forms t
        scala-font-lock:var-face font-lock-variable-name-face))

;;;; Mode detection.
(easy-auto-mode
 '((clojure-mode "build.boot\\'")
   (enh-ruby-mode "\\.rb\\'" "\\.rake\\'"
                  "Rakefile\\'" "Guardfile\\'" "Gemfile\\'" "Appraisals\\'"
                  "\\.gemspec\\'" "\\.?irbrc\\'" "\\.rabl\\'" "\\.ru\\'"
                  "\\.simplecov\\'")
   (js2-mode "\\.js\\'")
   (web-mode "\\.jsx\\'" "\\.html\\'")
   (markdown-mode "\\.md\\'" "\\.markdown\\'")
   (gitconfig-mode "gitconfig\\'")
   (gitignore-mode "gitignore\\'")
   (groovy-mode "Jenkinsfile\\'")))

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;;;; Some things were dropped from melpa.
(require 'quelpa)
(quelpa '(frame-fns :fetcher github :repo "emacsmirror/frame-fns"))
(quelpa '(frame-cmds :fetcher github :repo "emacsmirror/frame-cmds"))
(quelpa '(zoom-frm :fetcher github :repo "emacsmirror/zoom-frm"))

;;;; Start triggering the after-loads.
(require 'uniquify)
(require 'multiple-cursors)
(require 'company)
(require 'helm-config)
(require 'flycheck)

;;;; Show me what takes too long.
(save-current-buffer
  (switch-to-buffer "*scratch*" 'norecord)
  (goto-char (point-max))
  (insert (pp (pd/slowest-require-times))))
