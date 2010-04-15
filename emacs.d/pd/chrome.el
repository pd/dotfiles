; chrome is my default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")

; emacs chrome server lets me edit a textfield's content from emacs
; but only boot it if this is emacsd
(if (and (daemonp) (locate-library "edit-server"))
    (progn
      (require 'edit-server)
      (edit-server-start)))

(provide 'pd/chrome)
