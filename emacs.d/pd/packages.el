; el-get, the new hotness
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

; might be annoying; if el-get isn't present, autoinstalls. we'll see.
(unless (require 'el-get nil t)
  (message "Installing el-get...")
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(setq pd/el-get-packages
      (append
       '(buffer-move cheat clojure-mode coffee-mode color-theme el-get elein
         eredis full-ack inf-ruby magit mmm-mode org-mode package24
         profile-dotemacs rainbow-delimiters rainbow-mode rvm smex sudo-save
         tail yaml-mode yari scala-mode ensime)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync pd/el-get-packages)

(provide 'pd/packages)
