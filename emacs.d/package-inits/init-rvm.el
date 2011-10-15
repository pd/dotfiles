(rvm-autodetect-ruby)
(defadvice shell-dirstack-message (after rvm-on-shell-dirstack-message last activate)
  (rvm-activate-corresponding-ruby))
