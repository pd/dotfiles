(add-to-list 'after-init-hook 'rvm-autodetect-ruby)

(defadvice shell-dirstack-message (after rvm-on-shell-dirstack-message last activate)
  (rvm-activate-corresponding-ruby))

(defvar pd/rvm-latest-ruby nil "The most recently used ruby+gemset.")
(defadvice rvm-use (around rvm-use-only-if-different-ruby first (new-ruby new-gemset) activate)
  (let ((ruby (rvm--ruby-gemset-string new-ruby new-gemset)))
    (when (not (string= ruby pd/rvm-latest-ruby))
      ad-do-it
      (setq pd/rvm-latest-ruby ruby))))
