; save my buffers.
; losing 78 buffers cuz my laptop ran out of battery == sad pd.
(turn-on-save-visited-files-mode)

; how to open urls
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (cond ((pd/macosx-p) "open")
                                       ((executable-find "chromium-dev") "chromium-dev")
                                       ((executable-find "chromium") "chromium")
                                       t "firefox"))

; why does woman open its own frame by default?
(setq woman-use-own-frame nil)

; run-ruby etc should put the buffer in my current window,
; not a seemingly random different one ...
(nconc same-window-buffer-names '("*ruby*" "*js*"))

(provide 'pd/misc)
