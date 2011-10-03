; el-get, the new hotness (that is only a little bit hot...)
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
  (let (el-get-master-branch)
    (pd/eval-url
     "https://raw.github.com/dimitri/el-get/master/el-get-install.el")))

(setq el-get-verbose t)

(setq el-get-sources
      '((:name el-get
               :branch "master")

        (:name package
               :after (lambda ()
                        (setq package-archives
                              '(("ELPA" . "http://tromey.com/elpa/")
                                ("gnu" . "http://elpa.gnu.org/packages/")
                                ("marmalade" . "http://marmalade-repo.org/packages/")))))

        (:name magit
               :after (lambda ()
                        (global-set-key (kbd "C-M-g") 'magit-status)
                        (pd/magit-setup)))

        (:name save-visited-files
               :type git
               :url "https://github.com/nflath/save-visited-files.git"
               :after (lambda ()
                        (autoload 'turn-on-save-visited-files-mode "save-visited-files" "meh" t)
                        (turn-on-save-visited-files-mode)))

        (:name rspec-mode
               :url "https://github.com/earakaki/rspec-mode.git")

        (:name haml-mode
               :url "git://github.com/pd/haml-mode.git"
               :branch "wip")

        (:name buffer-move
               :after (lambda ()
                        (global-set-key (kbd "C-x w k") 'buf-move-up)
                        (global-set-key (kbd "C-x w j") 'buf-move-down)
                        (global-set-key (kbd "C-x w h") 'buf-move-left)
                        (global-set-key (kbd "C-x w l") 'buf-move-right)))))

(setq pd/el-get-packages
      (append
       '(el-get package
                magit smex buffer-move
                full-ack sudo-save tail cheat
                ; ruby-mode inf-ruby
                rvm
                rinari rspec-mode yari yaml-mode
                haml-mode sass-mode
                coffee-mode
                scala-mode ;ensime
                clojure-mode paredit elein
                eredis
                color-theme color-theme-sanityinc)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync pd/el-get-packages)

(provide 'pd/packages)
