(defadvice find-file-at-point (around find-file-at-point-line-number (&optional filename) activate)
  "Enhance find-file-at-point to support jumping directly to the line named by file:lineno.

It is very unlikely that this works for all the crap ffap can do. I don't care, I don't
use any of that."
  (let* ((sap    (ffap-string-at-point))
         (match  (s-match ":\\([0-9]+\\)$" sap))
         (lineno (and match (cadr match))))
    ad-do-it
    (when lineno
      (goto-char (point-min))
      (forward-line (1- (string-to-number lineno))))))
