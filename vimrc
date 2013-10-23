execute pathogen#infect()
syntax on
filetype plugin indent on

" what? in the old vimrc, seems important ...
" autocmd BufEnter * :syntax sync fromstart

set noerrorbells visualbell
set tabstop=2 shiftwidth=2 softtabstop=2 expandtab
set smartindent smarttab smartcase
set number
set nowrap
set lazyredraw
set hidden
set title mouse=a

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

" colors
if &t_Co > 2 || has("gui_running")
  set bg=dark
  colorscheme hemisu
endif
