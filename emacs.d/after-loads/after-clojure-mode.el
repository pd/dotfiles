(require 'nrepl)

(add-hook 'clojure-mode-hook 'pd/lisp-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

; this gets unset by pd/define-newline-and-indent. rather than unwind
; that trainwreck, i'll just manually restore the bindings.
(keydef (nrepl "RET") nrepl-return)
(keydef (nrepl "<return>") nrepl-return)
