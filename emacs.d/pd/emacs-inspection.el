;; misc shit for tinkering with emacs
(defun pd/select-symbols (pred)
  "Returns all elements from `obarray' matching PRED."
  (let (matches)
    (do-symbols (sym)
      (when (funcall pred sym)
        (!cons sym matches)))
    matches))

(defun pd/all-modes ()
  (sort (--filter (s-match "-mode$" (symbol-name it))
                  (pd/select-symbols 'commandp))
        (lambda (a b) (string< (symbol-name a) (symbol-name b)))))

(defun pd/major-modes ()
  (--reject (member it minor-mode-list) (pd/all-modes)))

(defun pd/active-minor-modes ()
  (--filter (and (boundp it) (symbolp it) (symbol-value it)) minor-mode-list))

(defun pd/autoload-p (sym)
  "t if SYM refers to an autoload function (ie, not yet loaded)"
  (let ((fun (symbol-function sym)))
    (and (listp fun)
         (equal 'autoload (car fun)))))

(defun pd/alias-p (sym)
  "t if SYM is an alias for another function"
  (let ((fun (symbol-function sym)))
    (and fun (symbolp fun) (not (equal sym fun)))))


;; figure out what's missing from my Carton
(defun pd/carton-dependencies (&optional carton)
  "Return the list of declared dependencies in your Carton file."
  (let (deps
        (carton (or carton (expand-file-name "Carton" user-emacs-directory))))
    (flet ((source (&rest args) nil)
           (depends-on (dep &optional version)
                       (setq deps (nconc deps (list dep)))))
      (with-temp-buffer
        (insert-file-contents carton)
        (eval-buffer))
      (nreverse deps))))

(defun pd/packages-not-in-carton ()
  (let ((carton-deps (mapcar 'intern (pd/carton-dependencies)))
        (installed   (mapcar 'car package-alist)))
    (--reject (member it carton-deps) installed)))

(defun pd/packages-with-dependencies ()
  (--reject (= 1 (length it))
            (--map (cons (car it) (package-desc-reqs (cdr it)))
                   package-archive-contents)))

(defun pd/packages-depending-on (package)
  (mapcar 'car
          (-select (lambda (p-and-dep-list)
                     (--any? (equal package (car it))
                             (cdr p-and-dep-list)))
                   (pd/packages-with-dependencies))))

;; "12 packages can be upgraded". Well, that's nice. Which ones?
(defun pd/name-package-upgrades ()
  (interactive)
  (let ((upgrades (package-menu--find-upgrades)))
    (when upgrades (message "Outdated: %s" (s-join ", " (mapcar 'symbol-name (mapcar 'car upgrades)))))))

(provide 'pd/emacs-inspection)
