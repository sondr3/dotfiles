filetype off

call plug#begin(stdpath('data') . '/plugged')

" Collection of language
Plug 'sheerun/vim-polyglot'
" Color themes
Plug 'junegunn/seoul256.vim'
Plug 'jacoborus/tender.vim'
" Auto-completion framework
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'fannheyward/coc-rust-analyzer', {'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-json', {'do': 'yarn install --frozen-lockfile'}
" A really neat generic finder
Plug 'liuchengxu/vim-clap'
" Linting, formatting etc
Plug 'dense-analysis/ale'
" Git gutters
Plug 'mhinz/vim-signify'
" Auto creating and closing pairs of things
Plug 'tmsvg/pear-tree'
" Show nice colors where you can #eee
Plug 'norcalli/nvim-colorizer.lua'
" A somewhat nice git interface
Plug 'tpope/vim-fugitive'
" Surrounds, like vim-surround, just better
Plug 'machakann/vim-sandwich'
" Comment out things
Plug 'tpope/vim-commentary'

call plug#end()

set termguicolors
syntax enable
colo tender

" if hidden is not set, TextEdit might fail.
set hidden

" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup

" relative line numbers, highlight current line
set number relativenumber
set cursorline

" Spaces, not tabs dammit
set tabstop=2
set shiftwidth=2
set expandtab

" Better display for messages
" set cmdheight=2

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=250

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

"This unsets the "last search pattern" register by hitting return
nnoremap <silent> <CR> :noh<CR>

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Or use `complete_info` if your vim support it, like:
" inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

" ale configuration
let g:ale_nix_nixpkgsfmt_executable = "nixfmt"
let g:ale_fixers = {'haskell': ['brittany', 'hlint'], 'nix': ['nixpkgs-fmt'], 'rust': ['rustfmt']}
let g:ale_fix_on_save = 1

" Lua configuration
lua require'colorizer'.setup()

" pear-tree
let g:pear_tree_ft_disabled = ['clap_input'] " don't break vim-clap
let g:pear_tree_smart_openers = 1
let g:pear_tree_smart_closers = 1
let g:pear_tree_smart_backspace = 1
