;;; pd/use-package-fmt.el --- :fmt use-package keyword -*- lexical-binding: t; -*-

;;; Commentary:
;; Formatter-on-save in one line:
;;   (use-package go-ts-mode :fmt (:program "goimports"))
;;   (use-package jsonnet-mode :fmt (:program "jsonnetfmt" :args ("-")))
;; The emitted `reformatter-define' needs reformatter loaded by the time the
;; using package's form is evaluated.

;;; Code:

(require 'use-package-core)

(defun use-package-normalize/:fmt (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (_label arg)
      (if (and (consp arg) (keywordp (car arg)))
          arg
        (use-package-error
         ":fmt wants (:program P [:args (ARG ...)])")))))

(defun use-package-handler/:fmt (name _keyword spec rest state)
  (let* ((mode   (if (string-suffix-p "-mode" (symbol-name name))
                     name
                   (intern (concat (symbol-name name) "-mode"))))
         (hook   (intern (concat (symbol-name mode) "-hook")))
         (fmt    (intern (concat "pd/" (symbol-name name) "-fmt")))
         (onsave (intern (concat (symbol-name fmt) "-on-save-mode"))))
    (use-package-concat
     (use-package-process-keywords name rest state)
     `((reformatter-define ,fmt
         :program ,(plist-get spec :program)
         :args (list ,@(plist-get spec :args)))
       (add-hook (quote ,hook) (function ,onsave))))))

(unless (memq :fmt use-package-keywords)
  (setq use-package-keywords
        (mapcan (lambda (kw) (if (eq kw :config) (list :fmt kw) (list kw)))
                use-package-keywords)))

(provide 'pd/use-package-fmt)
;;; pd/use-package-fmt.el ends here
