(defun pd/select-symbols (pred)
  "Returns all elements from `obarray' matching PRED."
  (let (matches)
    (do-symbols (sym)
      (when (funcall fn sym)
        (!cons sym matches)))
    matches))

(defun pd/all-modes ()
  (sort (--filter (s-match "-mode$" (symbol-name it))
                  (pd/select-symbols 'commandp))
        (lambda (a b) (string< (symbol-name a) (symbol-name b)))))

(defun pd/major-modes ()
  (--reject (member it minor-mode-list) (pd/all-modes)))

(defun pd/minor-modes ()
  (--filter (member it minor-mode-list) (pd/all-modes)))

(defun pd/autoload-p (sym)
  "t if SYM refers to an autoload function (ie, not yet loaded)"
  (let ((fun (symbol-function sym)))
    (and (listp fun)
         (equal 'autoload (car fun)))))

(defun pd/alias-p (sym)
  "t if SYM is an alias for another function"
  (let ((fun (symbol-function sym)))
    (and fun (symbolp fun) (not (equal sym fun)))))

(provide 'pd/emacs-inspection)
