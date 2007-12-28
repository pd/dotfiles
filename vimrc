" vim:fdm=marker

" :he <Leader>
"   <Leader>s -- clear search highlights
"   <Leader>p -- toggle paste mode
"   <Leader>n -- toggle line number
"   <Leader>r -- resource vimrc
"   <Leader>c,-- comment/uncomment a block
"   <Leader>u
"
"   <M-F10> -- echoes the name of the syntax element beneath the cursor
"
" :he cmdline-window
"   q:  -- command line window for : commands
"   q?, -- command line window for search commands
"   q?/
"
" regenerate documentation for ~/dotfiles/vim/doc
"   :helptags ~/dotfiles/vim/doc
"
" misc:
"   %/    -- in Command mode, expands to the path of the current file's
"            directory.

set nocompatible

colorscheme default
filetype plugin indent on " :he :filetype-overview

" basic options
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
set foldmethod=marker foldlevelstart=99

" :he :syn-sync-first
" simplest syntax syncing. can be slow
syntax on
au BufEnter * :syntax sync fromstart

" keymappings
map <silent> <Leader>s :noh<CR>
map <silent> <Leader>p :set paste!<CR>
map <silent> <Leader>n :set number!<CR>

map <M-F10> :echo synIDattr(synID(line('.'), col('.'), 1), 'name')<CR>
cmap %/ <C-R>=expand("%:p:h")."/"<CR>

" buffer view resizing: M-S-Right/Left/Up/Down {{{1
" technically probably won't work in console, but
" generic enough it's worth having outside of gvimrc
map <M-S-Right> :vertical resize +3<CR>
map <M-S-Left> :vertical resize -3<CR>
map <M-S-Up> :resize +3<CR>
map <M-S-Down> :resize -3<CR>

" vimrc resourcing {{{1
if !exists("*ResourceVim")
  function ResourceVim()
    source ~/.vimrc
    if has("gui_running")
      source ~/.gvimrc
    endif
  endfunction
endif
map <silent> <Leader>r :call ResourceVim()<CR>

" toggle folds with <SPACE> {{{1
" from http://vim.wikia.com/wiki/Toggle_a_fold_with_a_single_keystroke
" Toggle fold state between closed and opened.
"
" If there is no fold at current line, just moves forward.
" If it is present, reverse it's state.
fun! ToggleFold()
  if foldlevel('.') == 0
    normal! l
  else
    if foldclosed('.') < 0
      . foldclose
    else
      . foldopen
    endif
  endif
  " Clear status line
 echo
endfun
nnoremap <silent> <space> :call ToggleFold()<CR>
" 1}}}

" status line {{{1
set laststatus=2 " = always
set statusline=%<%f\%h%m%r%=%-20.(line=%l\ \ col=%c%V\ \ totlin=%L%)\ \ \ %h%m%r%=%-40(bytval=0x%B,%n%Y%)\%P
" 1}}}

" kill off vim7's matchparen plugin {{{1
let loaded_matchparen = 1
