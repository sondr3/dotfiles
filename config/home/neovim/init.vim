filetype off

call plug#begin(stdpath('data') . '/plugged')

Plug 'sheerun/vim-polyglot'
Plug 'jacoborus/tender.vim'

call plug#end()

set termguicolors
syntax enable
colorscheme tender
