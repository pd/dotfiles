(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(load-theme 'molokai t)
(load-theme 'despot t)

(require 'powerline)
(powerline-center-theme)

(provide 'pd/theme)
