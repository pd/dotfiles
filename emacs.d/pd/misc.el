; how to open urls
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (if (pd/macosx-p) "open"
                                   "chromium"))

; why does woman open its own frame by default?
(setq woman-use-own-frame nil)

; run-ruby etc should put the buffer in my current window,
; not a seemingly random different one ...
(nconc same-window-buffer-names '("*ruby*" "*js*"))

(provide 'pd/misc)
