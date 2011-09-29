; el-get, the new hotness
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(defun pd/eval-url (url)
  (let ((buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (eval-region (point) (point-max))
      (kill-buffer (current-buffer)))))

(unless (require 'el-get nil t)
  (pd/eval-url
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"))

(setq el-get-sources
      '((:name package24
               :after (lambda ()
                        (setq package-archives
                              '(("ELPA" . "http://tromey.com/elpa/")
                                ("gnu" . "http://elpa.gnu.org/packages/")
                                ("marmalade" . "http://marmalade-repo.org/packages/")
                                ("technomancy" . "http://repo.technomancy.us/emacs/")))))

        (:name save-visited-files
               :type git
               :url "https://github.com/nflath/save-visited-files.git"
               :after (lambda ()
                        (autoload 'turn-on-save-visited-files-mode "save-visited-files" "meh" t)
                        (turn-on-save-visited-files-mode)))

        (:name buffer-move
               :after (lambda ()
                        (global-set-key (kbd "C-x w k") 'buf-move-up)
                        (global-set-key (kbd "C-x w j") 'buf-move-down)
                        (global-set-key (kbd "C-x w h") 'buf-move-left)
                        (global-set-key (kbd "C-x w l") 'buf-move-right)))))

(setq pd/el-get-packages
      (append
       '(el-get package24
                magit smex color-theme org-mode buffer-move
                full-ack sudo-save tail cheat
                ruby-mode rvm inf-ruby rspec-mode yari yaml-mode
                haml-mode sass-mode
                coffee-mode
                scala-mode ensime
                clojure-mode paredit elein
                eredis
                color-theme-zenburn color-theme-zen-and-art color-theme-tomorrow color-theme-ir-black
                color-theme-twilight color-theme-solarized color-theme-subdued
                color-theme-almost-monokai color-theme-sanityinc color-theme-tango color-theme-tango-2
                color-theme-chocolate-rain color-theme-desert color-theme-railscasts)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync pd/el-get-packages)

(provide 'pd/packages)
