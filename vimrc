" vim:fdm=marker

" :he <Leader>
"   <Leader>s -- clear search highlights
"   <Leader>p -- toggle paste mode
"   <Leader>n -- toggle line number
"   <Leader>r -- resource vimrc
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
" command mode expansions:
"   %% -- the path to the current file
"   %/ -- the path to the current file's directory

set nocompatible

filetype plugin indent on " :he :filetype-overview

" :he :syn-sync-first
syntax on
au BufEnter * :syntax sync fromstart

" I use his Terminal.app theme, too, so this gels very well
set background=dark
colorscheme ir_black

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
set smartcase
set noerrorbells visualbell
set shortmess=atI
set title
set foldmethod=marker foldlevelstart=99
set modeline modelines=2
" set ruler " :he 'statusline', see note re:CTRL-G

" generic keymappings
map <silent> <Leader>s :nohlsearch<CR>
map <silent> <Leader>p :set paste!<CR>
map <silent> <Leader>n :set number!<CR>
map <M-F10> :echo synIDattr(synID(line('.'), col('.'), 1), 'name')<CR>
cmap %/ <C-R>=expand("%:p:h")."/"<CR>
cmap %% <C-R>=expand("%")<CR>

" status line and window title {{{1
" :he 'statusline'
" it's a beast.
set laststatus=2 " = always display the status line
set statusline=%3.3(%M%)\ buffer(%n):\ %f\%{GitBranchInfoString()}%*\ %y\ %=chr:\ %3b\ 0x%02B\ \|\ %P:\ ln\ %l/%L\ col\ %2c
set titlestring=vim:\ buf\ %n:\ %f\ %m
" 1}}}

" buffer view resizing: <M-S-{Right,Left,Up,Down}> {{{1
" technically probably won't work in console, but
" generic enough it's worth having outside of gvimrc
map <M-S-Right> :vertical resize +3<CR>
map <M-S-Left> :vertical resize -3<CR>
map <M-S-Up> :resize +3<CR>
map <M-S-Down> :resize -3<CR>

" vimrc resourcing with <Leader>r {{{1
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

" kill off vim7's matchparen plugin {{{1
let loaded_matchparen = 1

" misc plugin config {{{1
let NERDShutUp = 1

let g:git_branch_status_text=" gb: "
let g:git_branch_status_nogit=""
let g:git_branch_status_around=""
