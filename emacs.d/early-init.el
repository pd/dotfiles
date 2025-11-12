(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

; Don't burn 100% CPU just trying to print native compilation
; errors that I truly don't give a shit about.
(setq native-comp-async-report-warnings-errors 'silent)

; When that doesn't work out:
; (setq native-comp-deferred-compilation nil)
