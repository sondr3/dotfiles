" vim-plug settings
call plug#begin('~/.vim/plugged')

" Installed addons
Plug 'chriskempson/base16-vim'
Plug 'bling/vim-airline'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-surround'
Plug 'Shougo/unite.vim'
Plug 'Shougo/vimfiler.vim'
Plug 'jeffkreeftmeijer/vim-numbertoggle'
Plug 'mattn/emmet-vim'
Plug 'Lokaltog/vim-easymotion'
Plug 'kien/ctrlp.vim'
Plug 'raimondi/delimitMate'
Plug 'tmux-plugins/vim-tmux'
Plug 'christoomey/vim-tmux-navigator'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'

call plug#end()

" General settings
:imap jk <Esc>
filetype plugin indent on
syntax on
set mouse=a
set mousehide
set showcmd
set complete-=i
set smarttab
scriptencoding utf-8
set shortmess+=filmnrxoOtT
set viewoptions=folds,options,cursor,unix,slash
set virtualedit=onemore
set history=1000
set spell
set hidden
set iskeyword-=.
set iskeyword-=#
set iskeyword-=-
set nrformats-=octal
set autoread
set fileformats+=mac
set wildignore+=*/tmp/*,*.so,*.swp,*.zip

set ttimeout
set ttimeoutlen=100

" Useful defaults
" Trim whitespace when saving
autocmd BufWritePre * %s/\s\+$//e
" Return to last location when loading
autocmd BufReadPost * normal `"

" Keybindings
" Make <space> the leader button
let mapleader=","
" Change the command key for vim
nnoremap ; :
" Make vim scroll line-by-line, not around wrapped lines
:nmap j gj
:nmap k gk
" <Leader>o to open a new file
nnoremap <Leader>o :CtrlP<CR>
nnoremap <Leader>w :w<CR>

" Easier moving between windows
" Skips having to press C-w first
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Clear searches automatically
nmap <silent> ,/ :nohlsearch<CR>

" Interface
set term=screen-256color
set t_Co=256
let base16colorspace=256
colorscheme base16-solarized
highlight clear GitGutterAdd
highlight clear GitGutterChange
highlight clear GitGutterDelete
highlight clear GitGutterChangeDelete

" Sensible defaults from Tpope
if !&scrolloff
  set scrolloff=1
endif
if !&sidescrolloff
  set sidescrolloff=5
endif
set display+=lastline

if &encoding ==# 'latin1' && has('gui_running')
  set encoding=utf-8
endif

if has('path_extra')
  setglobal tags-=./tags tags^=./tags;
endif

if !empty(&viminfo)
  set viminfo^=!
endif
set sessionoptions-=options

" Allow color schemes to do bright colors without forcing bold.
if &t_Co == 8 && $TERM !~# '^linux'
  set t_Co=16
endif

inoremap <C-U> <C-G>u<C-U>

:let g:vimfiler_as_default_explorer = 1

set tabpagemax=50
set showmode

set cursorline
highlight clear SignColumn
highlight clear LineNr

set ruler
set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%)

if has('statusline')
    set laststatus=2
    set statusline=%<%f\
    set statusline+=%w%h%m%r
    set statusline+=%{fugitive#statusline()}
    set statusline+=\ [%{&ff}/%Y]
    set statusline+=\ [%{getcwd()}]
    set statusline+=%=%-14.(%l,%c%V%)\ %p%%
endif

set backspace=indent,eol,start
set linespace=0
set nu
set showmatch
set incsearch
set hlsearch
set winminheight=0
set ignorecase
set wildignorecase
set smartcase
set wildmenu
set wildmode=list:longest,full
set whichwrap=b,s,h,l,<,>,[,]
set scrolljump=5
set scrolloff=3
set foldenable
set list
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+

" Formatting
set nowrap
set autoindent
set shiftwidth=4
set expandtab
set tabstop=4
set softtabstop=4
set nojoinspaces
set splitright
set splitbelow
set pastetoggle=<F12>

" Markdown
au BufRead,BufNewFile *.md setlocal textwidth=80
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
let g:vim_markdown_frontmatter=1

" Airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline#extensions#tabline#left_sep = ''
let g:airline#extensions#tabline#left_alt_sep = ''

" ctrlp
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_show_hidden = 0
let g:ctrlp_custom_ignore = '\v[\/](node_modules|target|dist)|\v[\/]\.(git|hg|svn)$'
let g:ctrlp_working_path_mode = 'ra'

" delmitMate
let delimitMate_expand_cr = 2
