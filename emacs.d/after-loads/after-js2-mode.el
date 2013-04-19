(require 'slime-js)

(defun pd/run-swank-js ()
  (interactive)
  (message "Launching swank-js ...")
  (apply #'make-comint "swank-js" "swank-js" nil nil))

(defun pd/slime-js-node ()
  (interactive)
  (when (not (get-buffer "*swank-js*"))
    (pd/run-swank-js))
  (setq slime-protocol-version 'ignore)
  (slime-connect "localhost" 4005))

(defun pd/js2-setup ()
  (setq js2-basic-offset 2
        js2-bounce-indent-p nil
        js2-missing-semi-one-line-override t))

(add-hook 'js2-mode-hook 'pd/js2-setup)
(add-hook 'js2-mode-hook 'slime-js-minor-mode)

(keydef (js2 "C-c x j") 'pd/slime-js-node)
