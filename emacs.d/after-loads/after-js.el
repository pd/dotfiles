; function( -> ƒ(
; ty emacs-starter-kit
(font-lock-add-keywords
 'js-mode `(("\\(function *\\)("
             (0 (progn (compose-region (match-beginning 1)
                                       (match-end 1) "ƒ")
                       nil)))))

(setq js-indent-level 2
      js-auto-indent-flag nil)
