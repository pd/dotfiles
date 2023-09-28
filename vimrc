set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'noahfrederick/Hemisu'
Plugin 'fatih/molokai'
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'altercation/vim-colors-solarized'
Plugin 'tpope/vim-fugitive'
Plugin 'pangloss/vim-javascript'
Plugin 'tpope/vim-markdown'
Plugin 'cespare/vim-toml'
Plugin 'vim-ruby/vim-ruby'
Plugin 'tpope/vim-sensible'
Plugin 'tpope/vim-surround'
Plugin 'rust-lang/rust.vim'
Plugin 'racer-rust/vim-racer'
Plugin 'fatih/vim-go'
Plugin 'airblade/vim-gitgutter'
Plugin 'ntpeters/vim-better-whitespace'
Plugin 'hashivim/vim-terraform'
Plugin 'google/vim-jsonnet'
call vundle#end()

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
map <silent> <Leader>n :set number!<CR>:GitGutterToggle<CR>
cmap %/ <C-R>=expand("%:p:h")."/"<CR>
cmap %% <C-R>=expand("%")<CR>

noremap <leader>t :CtrlP<CR>
noremap <leader>b :CtrlPBuffer<CR>

" colors
colorscheme molokai
set background=dark

" fzf4lyfe
set runtimepath+=/usr/local/opt/fzf

" all via https://github.com/fatih/vim-go
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_fmt_command = "~/go/bin/goimports"

au FileType go nmap <leader>r <Plug>(go-run)
au FileType go nmap <leader>ds <Plug>(go-def-split)
au FileType go nmap <leader>dv <Plug>(go-def-vertical)
au FileType go nmap <leader>gd <Plug>(go-doc)
au FileType go nmap <leader>gv <Plug>(go-doc-vertical)
au FileType go nmap <leader>gb <Plug>(go-doc-browser)
au FileType go nmap <leader>i <Plug>(go-implements)

set hidden
let g:racer_cmd = $HOME."/.cargo/bin/racer"
let g:rustfmt_command = $HOME."/.cargo/bin/rustfmt"
let g:rustfmt_autosave = 1
let g:rustfmt_fail_silently = 1
let $RUST_SRC_PATH = $HOME."/vendor/rust/src"

" tidy up
autocmd BufWritePre * StripWhitespace
let g:terraform_fmt_on_save = 1

" direnv
augroup InitVimDirenv
  autocmd!
  autocmd BufWritePost .envrc silent !direnv allow %
augroup END
