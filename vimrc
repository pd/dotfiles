" vim:fdm=marker

" blasphemy:
" kyleh ~ % mate .vimrc

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
" c:
"   <F1>  -- :make
"   <F2>  -- :cnext
"
" misc:
"   %/    -- in Command mode, expands to the path of the current file's
"            directory.

set nocompatible

colorscheme default
filetype plugin indent on " :he :filetype-overview

" basic options {{{1
let mapleader=";"
set tabstop=2 shiftwidth=2 softtabstop=2 expandtab
set autoindent smartindent smarttab
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

set laststatus=2 " = always
set statusline=%<%f\%h%m%r%=%-20.(line=%l\ \ col=%c%V\ \ totlin=%L%)\ \ \ %h%m%r%=%-40(bytval=0x%B,%n%Y%)\%P

aug pdRuby " {{{1
	au!

	" rjs files
	au BufEnter *.rjs set ft=ruby

	" more&more rails. starting to need this as default.
	au FileType ruby set et sw=2 ts=2 sts=2
	au FileType eruby set et sw=2 ts=2 sts=2
	au FileType eruby set ai

	" prevents the annoyance of forcing # to the first column
  "	au FileType ruby inoremap # X#
	" au FileType eruby inoremap # X#

	" prolly should be using makeprg=... or some such, but this
	" is plenty fine for me.
	au FileType ruby map <F1> :!ruby %<Enter>
	au FileType ruby map <F2> :!rake<Enter>
aug END

aug pdJava " {{{1
	au!
	au FileType java map <silent> <Leader>c :s!^!//!<CR>
	au FileType java map <silent> <Leader>u :s!^//!!<CR>
aug END

aug pdC " {{{1
	au!
	au FileType c set cin
	au FileType cc set cin
	au FileType cpp set cin

	au FileType c map <silent> <Leader>c :s!^!//!<CR>
	au FileType c map <silent> <Leader>u :s!^//!!<CR>

	au FileType c set fdm=syntax
	au FileType cc set fdm=syntax
	au FileType cpp set fdm=syntax
	au FileType c map <F1> :make<Enter>
	au FileType c map <F2> :cnext<Enter>
	au FileType cpp map <F1> :make<Enter>
	au FileType cpp map <F2> :cnext<Enter>
aug END

aug pdLisp " {{{1
	au!
	au FileType lisp set et showmatch

	au FileType lisp map <silent> <Leader>c :s!^!;!<CR>
	au FileType lisp map <silent> <Leader>u :s!^;!!<CR>

	au FileType lisp map <F1> :!clisp %<CR>
aug end

aug pdWeb " {{{1
	au!
	au FileType css set ai et sw=2 ts=2 sts=2
	au FileType html set ai et sw=2 ts=2 sts=2
aug END
" 1}}}

" taglist config {{{1
let Tlist_Ctags_Cmd="/usr/local/bin/exctags"
map <silent> <F6> :TlistToggle<CR>

" vtreeexplorer config {{{1
let treeExplVertical=1
let treeExplWinSize=50
map <silent> <F12> :VSTreeExplore<CR>

" minibufexpl config {{{1
map <silent> <F5> :MiniBufExplorer<CR>
let g:miniBufExplSplitBelow=0
let g:miniBufExplModSelTarget=1
" 1}}}

" inkpot in 88 color terms {{{1
if ! has("gui_running") && &t_Co == 88
	colorscheme inkpot
	highlight Folded cterm=NONE ctermfg=black ctermbg=22
	highlight MBEVisibleNormal cterm=NONE ctermfg=black ctermbg=22
endif

" kill off vim7's matchparen plugin {{{1
let loaded_matchparen = 1
