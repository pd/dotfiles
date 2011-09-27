; el-get, the new hotness
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

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
                ruby-mode rvm inf-ruby yari yaml-mode
                coffee-mode
                scala-mode ensime
                clojure-mode paredit elein
                eredis)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync pd/el-get-packages)

(provide 'pd/packages)
