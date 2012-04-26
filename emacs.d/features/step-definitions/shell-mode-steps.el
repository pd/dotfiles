(Given "^I have a shell-mode buffer named \"\\(.+\\)\"$"
       (lambda (arg)
         ; lol, good enough!
         (shell)))

(Then "^the buffer major-mode should be \"\\(.+\\)\"$"
      (lambda (expected-mode)
        (assert (equal (symbol-name major-mode) expected-mode))))
