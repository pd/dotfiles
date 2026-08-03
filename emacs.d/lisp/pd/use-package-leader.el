;;; pd/use-package-leader.el --- :leader use-package keyword -*- lexical-binding: t; -*-

;;; Commentary:
;; Reclaim SPC for evil <leader> in modes where evil-collection binds it:
;;   (use-package magit :leader magit-mode-map ...)
;;   (use-package dired :ensure nil :leader dired-mode-map)
;; Re-installs the leader trigger (identical to what `evil-set-leader'
;; installs globally) into each named keymap's normal/visual/motion aux
;; keymap, at a precedence that outranks evil-collection's own SPC binding.
;; Emits `evil-define-key', which needs evil loaded by the time the using
;; package's form is evaluated (it self-defers until the keymap exists).

;;; Code:

(require 'use-package-core)

(defun use-package-normalize/:leader (_name _keyword args)
  args)

(defun use-package-handler/:leader (name _keyword maps rest state)
  (use-package-concat
   (use-package-process-keywords name rest state)
   (mapcar (lambda (map)
             `(evil-define-key '(normal visual motion) ,map (kbd "SPC")
                '(menu-item "" nil :filter (lambda (_) (key-binding [leader])))))
           maps)))

(unless (memq :leader use-package-keywords)
  (setq use-package-keywords
        (mapcan (lambda (kw) (if (eq kw :config) (list :leader kw) (list kw)))
                use-package-keywords)))

(provide 'pd/use-package-leader)
;;; pd/use-package-leader.el ends here
