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

  ;; something keeps forcing my $SHELL to /bin/bash so just unfuck that manually
  (when (file-executable-p "/usr/local/bin/zsh")
    (setenv "SHELL" "/usr/local/bin/zsh")
    (setq shell-file-name "/usr/local/bin/zsh"))

  ;; gank the $PATH from a login shell, in case I launched from the dock
  (require 'exec-path-from-shell)
  (require 'dash)
  (setq exec-path-from-shell-check-startup-files nil)
  (--each '("GOPATH" "GIT_COMMITTER_EMAIL" "GIT_AUTHOR_EMAIL")
    (add-to-list 'exec-path-from-shell-variables it))
  (exec-path-from-shell-initialize)

  ;; brew tap homebrew/cask-fonts && brew cask install font-fira-code-mono-nerd-font
  (set-face-attribute 'default nil :height 120 :family "FiraCode Nerd Font Mono")

  ;; current MBP spams a hideous warning exclamation point mid-screen
  ;; as its visual bell. so disable that *and* get emacs to fuck off with
  ;; its audio bell.
  (setq visible-bell nil
        ring-bell-function 'ignore)

  ;; I launch emacs client using an applescript, which sets cwd to /
  ;; Rather than learn applescript, I fix it here.
  (when (string= default-directory "/")
    (cd (getenv "HOME")))

  ;; osx does not offer ls --dired
  (after 'dired
    (setq dired-use-ls-dired nil)))

(provide 'pd/osx)
