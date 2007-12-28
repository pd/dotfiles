setlocal et sw=2 ts=2 sts=2
setlocal autoindent
setlocal foldmethod=syntax

map <buffer><silent> <Leader>c :s/^/#/<CR>
map <buffer><silent> <Leader>u :s/^#//<CR>
