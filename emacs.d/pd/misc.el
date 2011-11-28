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

; ibuffer grouping!
(setq ibuffer-saved-filter-groups
      '(("pd"
         ("zabxuq" (filename . "zabxuq"))
         ("bzork" (filename . "bzork"))
         ("sauce" (filename . "sauce"))
         ("el-get/recipes" (filename . "el-get/recipes"))
         ("el-get" (filename . "el-get"))
         ("emacs.d" (filename . "emacs.d"))
         ("dotfiles" (filename . "dotfiles"))
         ("gems" (filename . ".rvm/gems"))
         ("terms" (or (mode . term-mode)
                      (mode . shell-mode)))
         ("magit" (name . "\*magit"))
         ("erc" (mode . erc-mode))
         ("system" (or (name . "\*Help\*")
                       (name . "\*Apropos\*")
                       (name . "\*info\*")
                       (name . "\*Backtrace\*")
                       (name . "\*Completions\*")
                       (name . "\*Messages\*")
                       (name . "\*scratch\*"))))))

(setq ibuffer-show-empty-filter-groups nil)
(defvar pd/default-ibuffer-filter-group "pd")
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups pd/default-ibuffer-filter-group)))

; run-ruby etc should put the buffer in my current window,
; not a seemingly random different one ...
(nconc same-window-buffer-names '("*ruby*" "*js*"))

; boot emacschrome if we can
(when (require 'edit-server nil t)
  (edit-server-start))

; http://snarfed.org/automatically_close_completions_in_emacs_shell_comint_mode
(defun comint-close-completions ()
  "Close the comint completions buffer.
Used in advice to various comint functions to automatically close
the completions buffer as soon as I'm done with it. Based on
Dmitriy Igrishin's patched version of comint.el."
  (if comint-dynamic-list-completions-config
      (progn
        (set-window-configuration comint-dynamic-list-completions-config)
        (setq comint-dynamic-list-completions-config nil))))

(defadvice comint-send-input (after close-completions activate)
  (comint-close-completions))

(defadvice comint-dynamic-complete-as-filename (after close-completions activate)
  (if ad-return-value (comint-close-completions)))

(defadvice comint-dynamic-simple-complete (after close-completions activate)
  (if (member ad-return-value '('sole 'shortest 'partial))
      (comint-close-completions)))

(defadvice comint-dynamic-list-completions (after close-completions activate)
    (comint-close-completions)
    (if (not unread-command-events)
        ;; comint's "Type space to flush" swallows space. put it back in.
        (setq unread-command-events (listify-key-sequence " "))))

(provide 'pd/misc)
