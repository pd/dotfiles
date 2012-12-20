(require 'powerline)

(setq display-time-24hr-format t
      display-time-interval 30
      display-time-default-load-average nil
      battery-mode-line-format "%b%p%%%%") ; absurd % quantity due to powerline

(display-time-mode)
(display-battery-mode)

(setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (lhs (list
                                (powerline-raw "%*" nil 'l)
                                (powerline-raw (if server-process
                                                   (propertize " S" 'help-echo "Connected to emacsd")
                                                 (propertize " D" 'help-echo "No server running")) nil 'r)
                                (powerline-buffer-size nil 'l)
                                (powerline-buffer-id nil 'l)

                                (powerline-raw "@ %lx%c" nil 'l)

                                (powerline-raw " ")
                                (powerline-arrow-right nil face1)

                                (when (boundp 'erc-modified-channels-object)
                                  (powerline-raw erc-modified-channels-object
                                                 face1 'l))

                                (powerline-major-mode face1 'l)
                                (powerline-process face1)
                                (powerline-minor-modes face1 'l)
                                (powerline-narrow face1 'l)

                                (powerline-raw " " face1)
                                (powerline-arrow-right face1 face2)

                                (powerline-vc face2)))

                          (rhs (list
                                (powerline-arrow-left face2 nil)
                                (powerline-raw (concat " " battery-mode-line-string) nil 'r)

                                (powerline-arrow-left nil face1)
                                (powerline-raw display-time-string face1 'l)

                                (powerline-raw "  " face1))))

                     (concat
                      (powerline-render lhs)
                      (powerline-fill face2 (powerline-width rhs))
                      (powerline-render rhs))))))

(provide 'pd/mode-line)
