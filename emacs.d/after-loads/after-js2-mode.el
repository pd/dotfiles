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
              js2-include-browser-externs nil
              js2-idle-timer-delay 0.5
              js2-skip-preprocessor-directives t) ; aka, ignore #!env node

(add-hook 'js2-mode-hook 'slime-js-minor-mode)

(keydef (js2 "RET")      js2-line-break)
(keydef (js2 "<return>") js2-line-break)

(keydef (js2 "C-c x j") pd/slime-js-node)
(keydef (js2 "M-j")     (join-line 1))

(js2r-add-keybindings-with-prefix "C-c C-r")
(js2r-add-keybindings-with-prefix "C-c r")

(defun pd/declare-common-js2-globals ()
  ; TODO when buffer matches /*global (\w+)*/ add $1
  ; see https://github.com/dgoodlad/emacs.d/blob/ed5188/conf/init-js2-mode.el#L3
  (when (string-match-p "_test\\.js" (buffer-file-name))
    (setq js2-additional-externs '("describe" "context" "it"))))

(add-hook 'js2-mode-hook 'pd/declare-common-js2-globals)


; Reserved words have changed over time; see Section 7.6.1 of
; http://www.ecma-international.org/ecma-262/5.1/Ecma-262.pdf
(setq js2-reserved-words
      ; some are already in js2-keywords
      '(class
        ; const
        enum
        export
        extends
        import
        super
        implements
        interface
        package
        private
        protected
        public
        static)

      ; ugh, rebuild a cache ...
      js2-reserved-word-names
      (let ((table (make-hash-table :test 'equal)))
        (loop for k in js2-reserved-words
              do
              (puthash (symbol-name k) 'js2-RESERVED table))
        table))
