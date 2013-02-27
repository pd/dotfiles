; how to open urls
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (cond ((pd/macosx-p) "open")
                                       ((executable-find "chromium") "chromium")
                                       ((executable-find "chromium-dev") "chromium-dev")
                                       (t "firefox")))

; why does woman open its own frame by default?
(setq woman-use-own-frame nil)

; help shouldn't ever steal focus
(setq help-window-select nil)

; stop prompting me every time i open a gemspec
(setq safe-local-variable-values
      (append '((encoding . utf-8)) safe-local-variable-values))

; run-ruby etc should put the buffer in my current window,
; not a seemingly random different one ...
(setq same-window-buffer-names '("*ruby*" "*js*"))

; only fontify given some idle time;
; speeds up spitting out tons of cucumber scenarios etc. into a shell-mode buffer
(setq jit-lock-defer-time 0.05)

(provide 'pd/misc)
