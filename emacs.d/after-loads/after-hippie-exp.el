; strictly specify the order of hippie-expand's search
; for autocompletion targets; eg, try a simple symbol
; completion from the line above this one before trying
; to complete an entire list from a buffer i opened last
; January.
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev-all-buffers
        try-expand-list
        try-expand-line
        try-expand-list-all-buffers
        try-expand-line-all-buffers))
