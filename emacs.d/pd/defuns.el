;; Functions common to many, many mode-hooks.
(defun pd/turn-off-show-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(defun pd/local-newline-and-indent ()
  (local-set-key (kbd "<return>") 'newline-and-indent))

(defun pd/turn-on-hl-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|FIX\\|TODO\\|HACK\\|XXX\\|REFACTOR\\)"
          1 font-lock-warning-face t))))

(defun pd/turn-off-comint-echo ()
  (setq comint-process-echoes t))


;; superior mark handling
; stolen from http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/
(defun pd/push-mark ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun pd/jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

;; vi's o and O
(defun pd/insert-newline ()
  (funcall (or (local-key-binding (kbd "<return>"))
               (key-binding (kbd "RET")))))

(defun pd/append-and-move-to-new-line ()
  "Inserts a blank line after the current one, and moves to it"
  (interactive)
  (end-of-line)
  (pd/insert-newline))

(defun pd/prepend-and-move-to-new-line ()
  "Inserts a blank line before the current one, and moves to it"
  (interactive)
  (if (= 1 (line-number-at-pos))
      (progn
        (beginning-of-buffer)
        (open-line 1)
        (beginning-of-buffer))
    (progn
      (previous-line)
      (pd/append-and-move-to-new-line))))


;; font size scaling
; preferable to text-scale-increase etc, as it operates on the frame
; instead of only the current buffer.
(defun pd/modify-font-size (amount)
  "Increase/decrease this frame's font size by AMOUNT"
  (set-face-attribute 'default nil
                      :height
                      (+ (face-attribute 'default :height)
                         amount)))

(defun pd/increase-font-size ()
  "Modify font size +20"
  (interactive)
  (pd/modify-font-size 20))

(defun pd/decrease-font-size ()
  "Modify font size -20"
  (interactive)
  (pd/modify-font-size -20))


;; Helpful misc
(defun pd/macosx-p ()
  "t if on a darwin system"
  (string-equal "darwin" system-type))

(defun pd/back-to-indentation-or-beginning-of-line ()
  "Moves point back to indentation if there is any
non blank characters to the left of the cursor.
Otherwise point moves to beginning of line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun xmpfilter ()
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region beg end "xmpfilter" nil 'replace)))

; from: http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

; from: xale@#emacs
(defun reload-buffer ()
  "Kill the current buffer and immediately reload it without moving point.
Useful for rerunning mode hooks."
  (interactive)
  (let ((path (buffer-file-name)) (point (point)))
    (kill-buffer)
    (find-file path)
    (goto-char point)))

; from: emacs-prelude
(defun pd/google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google: "))))))

(provide 'pd/defuns)
