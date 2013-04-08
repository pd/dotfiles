(require 'ibuffer-vc)

(setq ibuffer-default-sorting-mode 'filename/process
      ibuffer-show-empty-filter-groups nil
      ibuffer-formats '((mark modified read-only vc-status-mini " "
                              (name 18 18 :left :elide)
                              " "
                              (size 9 -1 :right)
                              " "
                              (mode 16 16 :left :elide)
                              " "
                              (vc-status 16 16 :left)
                              " "
                              filename-and-process)))

(defun pd/prepare-ibuffer ()
  (ibuffer-auto-mode 1)
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

(add-hook 'ibuffer-mode-hook 'pd/prepare-ibuffer)
