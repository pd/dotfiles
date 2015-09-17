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

  ;; brew tap caskroom/homebrew-fonts && brew install font-sauce-code-pro-for-powerline
  (set-face-attribute 'default nil :height 120 :family "Source Code Pro for Powerline")

  ;; I launch emacs client using an applescript, which sets cwd to /
  ;; Rather than learn applescript, I fix it here.
  (when (string= default-directory "/")
    (cd (getenv "HOME")))

  ;; osx does not offer ls --dired
  (after 'dired
    (setq dired-use-ls-dired nil)))

(provide 'pd/osx)
