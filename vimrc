set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'noahfrederick/Hemisu'
Plugin 'fatih/molokai'
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/nerdcommenter'
Plugin 'godlygeek/tabular'
Plugin 'kchmck/vim-coffee-script'
Plugin 'altercation/vim-colors-solarized'
Plugin 'tpope/vim-cucumber'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-haml'
Plugin 'pangloss/vim-javascript'
Plugin 'tpope/vim-markdown'
Plugin 'cespare/vim-toml'
Plugin 'bling/vim-airline'
Plugin 'vim-ruby/vim-ruby'
Plugin 'tpope/vim-sensible'
Plugin 'tpope/vim-surround'
Plugin 'rust-lang/rust.vim'
Plugin 'racer-rust/vim-racer'
Plugin 'fatih/vim-go'
Plugin 'mxw/vim-jsx'
Plugin 'rking/ag.vim'
Plugin 'majutsushi/tagbar'
Plugin 'airblade/vim-gitgutter'
Plugin 'elixir-lang/vim-elixir'
Plugin 'ntpeters/vim-better-whitespace'
Plugin 'hashivim/vim-terraform'
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
noremap <Leader>c :TagbarToggle<CR>
noremap <leader>d :NERDTreeToggle<CR>
noremap <leader>f :NERDTreeFind<CR>
noremap <leader>a :Ag<space>

" powerline fonts suck.
let g:airline_left_sep=''
let g:airline_left_alt_sep=''
let g:airline_right_sep=''
let g:airline_right_alt_sep=''

" colors
colorscheme molokai
set background=dark

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

" eventually i'll actually learn modern rust
set hidden
let g:racer_cmd = $HOME."/bin/multirustracer"
let g:ycm_rust_src_path = $HOME."/vendor/rust/src"
let g:rustfmt_command = $HOME."/bin/multirustfmt"
let g:rustfmt_autosave = 1
let $RUST_SRC_PATH = $HOME."/vendor/rust/src"

" despite https://github.com/rust-lang/rust.vim/issues/46
" and https://github.com/rust-lang/rust.vim/blob/9924277/README.md#using-vundle
" filetype detection simply doesn't work. i dunno. who cares. set it manually.
autocmd BufNewFile,BufRead *.rs set filetype=rust

" tidy up
autocmd BufWritePre * StripWhitespace
let g:terraform_fmt_onsave = 1
