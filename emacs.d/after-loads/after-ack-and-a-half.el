;; The default interactive behavior of ack-and-a-half is the opposite
;; of my workflow.  The prefix argument toggles whether to search by
;; regexp or literal string, but it's much more important to be able
;; to choose a directory interactively.
;;
;; So swap 'em. Prolly breaks some assumptions of ack-and-a-half.

(defun pd/ack--dir ()
  (if current-prefix-arg
      (let ((ack-and-a-half-prompt-for-directory t))
        (ack-and-a-half-read-dir))
    (ack-and-a-half-read-dir)))

(defun pd/ack (pattern &optional directory)
  (interactive
   (list (ack-and-a-half-read ack-and-a-half-regexp-search)
         (pd/ack--dir)))
  (ack-and-a-half pattern ack-and-a-half-regexp-search directory))

(defun pd/ack-same (pattern &optional directory)
  (interactive
   (list (ack-and-a-half-read ack-and-a-half-regexp-search)
         (pd/ack--dir)))
  (ack-and-a-half-same pattern ack-and-a-half-regexp-search directory))

(defalias 'ack 'pd/ack)
(defalias 'ack-same 'pd/ack-same)
