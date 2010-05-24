(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))

(defun pd/add-js-bindings ()
  (pd/enable-newline-and-indent js-mode-map))

(setq js-indent-level 2
      js-auto-indent-flag nil)

(add-hook 'js-mode-hook 'pd/run-coding-hook)
(add-hook 'js-mode-hook 'pd/add-js-bindings)

; js-comint on rhino
(autoload 'run-js "js-comint" "Javascript REPL" t)
(setq inferior-js-program-command "java -jar /home/pd/dotfiles/emacs.d/vendor/js.jar")

; mozrepl
(defun pd/enable-moz-minor-mode ()
  (moz-minor-mode 1))

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'js-mode-hook 'pd/enable-moz-minor-mode)

; function( -> ƒ(
; ty emacs-starter-kit
(eval-after-load 'js
  '(progn (font-lock-add-keywords
           'js-mode `(("\\(function *\\)("
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "ƒ")
                                 nil)))))))

(provide 'pd/js)
