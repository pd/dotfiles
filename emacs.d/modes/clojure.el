(require 'clojure-auto)
(require 'swank-clojure-autoload)
(require 'slime-autoloads)

(swank-clojure-config
 (setq swank-clojure-jar-path "/Users/kyleh/vendor/clojure/clojure.jar"))

(add-hook 'clojure-mode-hook
          (lambda ()
            (define-key clojure-mode-map (kbd "C-c C-b") 'lisp-eval-buffer)))

(add-hook 'inferior-lisp-mode-hook
          (lambda ()
            (define-key slime-mode-map (kbd "C-M-/") 'slime-complete-symbol)))
