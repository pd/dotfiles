set mouse=a
set columns=130
set lines=50

colorscheme autumnleaf
" no one ever styles the completion menu
hi Pmenu gui=none guifg=#002200 guibg=#ddeedd
hi PmenuSel gui=bold guifg=black guibg=#ffeebb

" resize the window using Control+Shift+[Arrow]
map <C-S-Left> :set columns-=5<CR>
map <C-S-Right> :set columns+=5<CR>
map <C-S-Up> :set lines-=5<CR>
map <C-S-Down> :set lines+=5<CR>
