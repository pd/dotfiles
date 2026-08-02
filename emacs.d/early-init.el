(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; Don't burn 100% CPU just trying to print native compilation
;; errors that I truly don't give a shit about.
(setq native-comp-async-report-warnings-errors 'silent)

;; When that doesn't work out:
;; (setq native-comp-deferred-compilation nil)

;; prefer updated init.el to the precompiled version
(setq load-prefer-newer t)

;; no GC during startup; GC less frequently after
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 50 1024 1024))))

;; disable some chrome before a frame exists
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
