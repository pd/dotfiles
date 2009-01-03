(require 'clojure-auto)
(require 'swank-clojure-autoload)
(require 'slime-autoloads)

(slime-setup '(slime-repl))

(swank-clojure-config
 (setq swank-clojure-binary "~/bin/clj")
 (setq swank-clojure-jar-path "~/.clj/clojure.jar")
 (add-to-list 'swank-clojure-extra-classpaths "~/sandbox/clojoku/src")
 (add-to-list 'swank-clojure-extra-classpaths "~/sandbox/clojoku/test"))

(add-hook 'clojure-mode-hook
          (lambda ()
            (define-key clojure-mode-map (kbd "C-c C-b") 'lisp-eval-buffer)
            (setq indent-tabs-mode nil)))

(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (define-key slime-mode-map (kbd "C-M-/") 'slime-complete-symbol)
            (setq indent-tabs-mode nil)))
