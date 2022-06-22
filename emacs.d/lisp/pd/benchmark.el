;; stolen from purcell's config:
;; https://github.com/purcell/emacs.d/commit/ac3636d

(defun pd/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defvar pd/require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun pd/slowest-require-times ()
  "Return `pd/require-times' sorted by descending length."
  (let ((times (copy-sequence pd/require-times)))
    (sort times (lambda (a b) (> (cdr a) (cdr b))))))

(defadvice require
  (around build-require-times (feature &optional filename noerror) activate)
  "Note in `pd/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (add-to-list 'pd/require-times
                     (cons feature
                           (pd/time-subtract-millis (current-time)
                                                    require-start-time))
                     t)))))

(provide 'pd/benchmark)
