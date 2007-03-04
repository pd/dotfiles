" vim:fdm=marker

" :he <Leader>
"   <Leader>s -- clear search highlights
"   <Leader>p -- toggle paste mode
"   <Leader>n -- toggle line number
"   <Leader>r -- resource vimrc
"   <Leader>c,-- comment/uncomment a block
"   <Leader>u
"	<Leader>a -- expands to ":Align "
"	    :he align.txt
"
"   <F5>  -- toggle buffer list display
"   <F6>  -- toggle tag list display
"   <F9>  -- cd's to directory of current buffer
"   <F10> -- echoes the name of the syntax element beneath the cursor
"   <F11> -- enters ii mode
"   <F12> -- opens a directory explorer in a vsplit window (:VSTreeExplore)
"
" ruby:
"   <F1>  -- runs the current file through the ruby interpreter (:!ruby %)
"   <F2>  -- runs 'rake'
"   <F3>  -- runs 'rake showspecs'
"   <F4>  -- runs 'rake cov'
"
" misc:
"   %/   -- in Command mode, expands to the path of the current file's
"           directory.

colorscheme adam
filetype plugin indent on " :he :filetype-overview

" use 88 colors in urxvt {{{1
if &term == "rxvt"
	set t_Co=88
elseif &term == "screen"
	set t_Co=88
elseif &term == "xterm-color"
	set t_Co=256
elseif &term == "cons25"
	set t_Co=8
else
	set t_Co=8
endif

" basic options {{{1
let mapleader=";"
set tabstop=4 shiftwidth=4 softtabstop=4
set autoindent smartindent noexpandtab smarttab
set formatoptions+=croql
set cinoptions+=:0
set cinkeys-=0#
set nocindent
set nowrap
set number
set lazyredraw
set ruler
set smartcase
set noerrorbells visualbell
set shortmess=atI
set title titleold=despot
set fdm=marker fdl=0
" set mouse=a     " mouse use in terms

" simplest syntax syncing. can be slow. {{{1
" :he :syn-sync-first
syntax on
au BufEnter * :syntax sync fromstart

" keymappings {{{1
map <silent> <Leader>s :noh<CR>
map <silent> <Leader>p :set paste!<CR>
map <silent> <Leader>n :set number!<CR>
map <silent> <Leader>c :s/^/#/<CR>
map <silent> <Leader>u :s/^#//<CR>
map <silent> <Leader>r :source ~/.vimrc<CR>
map <Leader>e :e <C-R>=expand("%:p:h")<CR>
map <Leader>a :Align 
map <silent> <F9> :cd %:p:h<CR>
map <F10> :echo synIDattr(synID(line('.'), col('.'), 1), 'name')<CR>
cmap %/ <C-R>=expand("%:p:h")."/"<CR>

" abbreviations {{{1
iab pdiish #!/bin/sh<CR>. $HOME/irc/pdii/pdii.sh.include
" 1}}}

aug pdJava " {{{1
	au!
	au FileType java map <silent> <Leader>c :s!^!//!<CR>
	au FileType java map <silent> <Leader>u :s!^//!!<CR>
aug END

aug pdC " {{{1
	au!
	au FileType c  set cin
	au FileType cc set cin
	au FileType h  set cin

	au FileType c map <silent> <Leader>c :s!^!//!<CR>
	au FileType c map <silent> <Leader>u :s!^//!!<CR>

	au FileType c set fdm=syntax
aug END

aug pdWiki " {{{1
	au!
	au FileType wiki setf wikipedia
aug END

aug pdLisp " {{{1
	au!
	au FileType lisp set et showmatch

	au FileType lisp map <silent> <Leader>c :s!^!;!<CR>
	au FileType lisp map <silent> <Leader>u :s!^;!!<CR>
aug end

aug pdHaskell " {{{1
	au!
	au FileType haskell set et showmatch

	au FileType haskell map <silent> <Leader>c :s!^!--!<CR>
	au FileType haskell map <silent> <Leader>u :s!^--!!<CR>
aug end

aug pdRuby " {{{1
	au!

	" prevents the annoyance of forcing # to the first column
	au FileType ruby inoremap # X#

	" prolly should be using makeprg=... or some such, but this
	" is plenty fine for me.
	au FileType ruby map <F1> :!ruby %<Enter>
	au FileType ruby map <F2> :!rake<Enter>
	au FileType ruby map <F3> :!rake showspecs<Enter>
	au FileType ruby map <F4> :!rake cov<Enter>
aug END

aug pdRSpecs " {{{1
	" turned out i didn't want simplefold
	au!
	au BufEnter *_spec.rb let b:simplefold_expr = 
		\'\v(^\s*(context|def|class|module|attr_reader|attr_accessor|alias_method|' .
			\   'attr|module_function' . ')\s' . 
		\ '|\v^\s*(public|private|protected)>' .
		\ '|^\s*\w+attr_(reader|accessor)\s|^\s*[#%"0-9]{0,4}\s*\{\{\{[^{])' .
		\ '|^\s*[A-Z]\w+\s*\=[^=]|^__END__$'
	au BufEnter *_spec.rb let b:simplefold_nestable_start_expr = 
		\ '\v^\s*(setup>|specify>|def>|if>|unless>|while>.*(<do>)?|' . 
		\         'until>.*(<do>)?|case>|for>|begin>)' .
		\ '|^[^#]*.*<do>\s*(\|.*\|)?'
	au BufEnter *_spec.rb let b:simplefold_nestable_end_expr = 
		\ '\v^\s*end'
	au BufEnter *_spec.rb let b:simplefold_prefix='\v^\s*(#.*)?$'
aug END
" 1}}}

" bufferlist config {{{1
hi BufferSelected ctermfg=darkcyan ctermbg=black cterm=bold
hi BufferNormal   ctermfg=white    ctermbg=black cterm=NONE
map <silent> <F5> :call BufferList()<CR>

" taglist config {{{1
let Tlist_Ctags_Cmd="/usr/local/bin/exctags"
map <silent> <F6> :TlistToggle<CR>

" vtreeexplorer config {{{1
let treeExplVertical=1
let treeExplWinSize=50
map <silent> <F12> :VSTreeExplore<CR>

" minibufexpl config {{{1
let g:miniBufExplModSelTarget=1
" 1}}}

" fix inkpot's Folded color in term. {{{1
if ! has("gui_running")
	highlight Folded cterm=NONE ctermfg=black ctermbg=22
endif

" kill off vim7's matchparen plugin {{{1
let loaded_matchparen = 1
