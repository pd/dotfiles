(require 'clojure-auto)
(require 'swank-clojure-autoload)
(require 'slime-autoloads)

(define-project-type clojure (generic)
  (look-for "src/*.clj"))

(slime-setup '(slime-repl))

(swank-clojure-config
 (setq swank-clojure-binary "~/bin/clj")
 (setq swank-clojure-jar-path "~/.clj/clojure.jar")
 (add-to-list 'swank-clojure-extra-classpaths "~/clj/clojoku/src")
 (add-to-list 'swank-clojure-extra-classpaths "~/clj/clojoku/test"))

(add-hook 'clojure-mode-hook
          (lambda ()
            (define-key clojure-mode-map (kbd "C-c b") 'slime-eval-buffer)
            (define-key clojure-mode-map (kbd "C-x C-e") 'slime-eval-last-expression)
            (define-key clojure-mode-map (kbd "C-x M-e") 'slime-eval-print-last-expression)
            (setq indent-tabs-mode nil)))
