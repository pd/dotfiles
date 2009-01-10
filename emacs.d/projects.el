(define-project-type ruby (generic)
  (or (look-for "Rakefile")
      (look-for "src/*.rb")
      (look-for "spec/*.rb")))

(define-project-type rails (ruby)
  (look-for "config/environment.rb"))

(define-project-type emacs-d (generic)
  (look-for "init.el"))

(define-project-type clojure (generic)
  (look-for "src/*.clj"))

(define-project-type oly-app (rails)
  (eproject--scan-parents-for file
                              (lambda (dir)
                                (or (string-match "oly-dev/$" (file-name-directory dir)) nil))))
