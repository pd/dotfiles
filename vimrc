execute pathogen#infect()
syntax on
filetype plugin indent on

set noerrorbells visualbell
set tabstop=2 shiftwidth=2 softtabstop=2 expandtab
set smartindent smarttab smartcase
set number
set nowrap
set lazyredraw
set hidden
set title
set mouse=

" fkn .swp
set backupdir=~/dotfiles/vim-tmp
set undodir=~/dotfiles/vim-tmp
set directory=~/dotfiles/vim-tmp

let mapleader=";"
map K <Nop>
map Y y$
map <silent> <Leader>s :nohlsearch<CR>
map <silent> <Leader>p :set paste!<CR>
map <silent> <Leader>n :set number!<CR>
cmap %/ <C-R>=expand("%:p:h")."/"<CR>
cmap %% <C-R>=expand("%")<CR>

noremap <leader>t :CtrlP<CR>
noremap <leader>b :CtrlPBuffer<CR>
noremap <leader>d :NERDTreeToggle<CR>
noremap <leader>f :NERDTreeFind<CR>
noremap <leader>a :Ag<space>

" powerline fonts suck.
let g:airline_left_sep=''
let g:airline_left_alt_sep=''
let g:airline_right_sep=''
let g:airline_right_alt_sep=''

" colors
if &t_Co > 2 || has("gui_running")
  colorscheme hemisu
  set bg=dark
endif
