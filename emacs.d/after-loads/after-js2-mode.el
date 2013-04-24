(require 'slime)
(require 'slime-js)

(defun pd/slime-js-node ()
  (interactive)
  (when (not (get-buffer "*swank-js*"))
    (message "Launching swank-js ...")
    (apply #'make-comint "swank-js" "swank-js" nil nil)
    (sleep-for 1))
  (setq slime-protocol-version 'ignore)
  (slime-connect "localhost" 4005))

(setq-default js2-basic-offset 2
              js2-bounce-indent-p nil
              js2-missing-semi-one-line-override t
              js2-include-node-externs t
              js2-include-browser-externs nil)

(add-hook 'js2-mode-hook 'slime-js-minor-mode)

(keydef (js2 "C-c x j") 'pd/slime-js-node)
(keydef (js2 "M-j")     (join-line 1))

(js2r-add-keybindings-with-prefix "C-c C-r")
(js2r-add-keybindings-with-prefix "C-c r")
