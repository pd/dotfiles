" Bart Trojanowski <bart@jukie.net>
"
" git:file.vim v0.1
"
" This will handle git show rev:file files when you open <branch>:<file> patterns.
" You can do both
"       $ vim HEAD~10:file
" and
"       :e branch:file
"
" Idea based on file:line.vim by Victor Bogado da Silva Lins.
"
" If you modify this file, please email me with your improvements.
"
" Install in ~/.vim/plugins/git:file.vim
"
" TODO:
"   - handle relative paths (right now you have to be at top of working dir)
"   - tab completion for :e (is that even possible?)

function! s:gotoline()
	let buf = bufnr("%")
	let file = bufname("%")

	if ! filereadable(file) && line('$')==1
                let cmd = "git show " . file
		exec 'normal :r!LANG=C ' . cmd . "\n:1d\n"

                " TODO: the mode of the file is not set to RO/no-modify
                "       but I cannot figure out why
                exec 'normal :set readonly nomodifiable\n'
	endif

endfunction

autocmd! BufNewFile [^~/]*:*,[^~/]*:**/*,[^~/]**/*:*,[^~/]**/*:**/* call s:gotoline()
