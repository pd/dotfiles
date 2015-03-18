(when (string-equal "darwin" system-type)
  (require 'bind-key)

  ;; Modifiers!
  (setq ns-command-modifier   'meta
        ns-alternate-modifier 'super
        ns-function-modifier  'hyper)

  ;; full screen that doesn't suck
  (setq ns-use-native-fullscreen nil)
  (bind-key "s-S-<return>" 'toggle-frame-fullscreen)

  ;; I am assured that sRGB is the Right Thing To Do(tm)
  (setq ns-use-srgb-colorspace t)

  ;; Stop asking me to print.
  (unbind-key "s-p")

  ;; gank the $PATH from a login shell, in case I launched from the dock
  (require 'exec-path-from-shell)
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (exec-path-from-shell-initialize)

  ;; Dunno a better way to do this. Sets the font to (roughly) Monaco 10pt.
  ;; Default was 12pt.
  (set-face-attribute 'default nil :height 100)

  ;; I launch emacs client using an applescript, which sets cwd to /
  ;; Rather than learn applescript, I fix it here.
  (when (string= default-directory "/")
    (cd (getenv "HOME")))

  ;; osx does not offer ls --dired
  (after 'dired
    (setq dired-use-ls-dired nil)))

(provide 'pd/osx)
