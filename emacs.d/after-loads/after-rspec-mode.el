(defun pd/spec-focus ()
  "Toggle 'focus: true' option on describe block"
  (interactive)
  (save-excursion
    (search-backward "describe ")
    (if (re-search-forward "focus: true" (line-end-position) 'noerror)
        (progn
          (delete-backward-char (length ", focus: true")))
      (progn
        (search-backward " do")
        (insert ", focus: true")))))

(keydef (rspec "C-c , f") pd/spec-focus)
