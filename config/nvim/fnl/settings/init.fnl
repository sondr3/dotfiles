(import-macros {: set! } :settings.macros)

; always UTF8
(set! fileencoding :utf-8)
; Incremental live completion, default on `master`
(set! inccommand :nosplit)
; Use system clipboard
(set! clipboard (+ vim.opt.clipboard :unnamedplus))

; Highlight searches when done searching
(set! hlsearch)

; Hide the `-- INSERT --` messages
(set! noshowmode)

;; enable relative line numbers
(set! number relativenumber)

; Increase width of number columns and always show sign column
(set! numberwidth 4 signcolumn :yes)

; For faster completion
(set! updatetime 250 timeoutlen 500)

; Set completeopt to have a better completion experience
(set! completeopt [:menu :menuone :noselect])

; Tabs vs spaces, make indenting great again
(set! smartindent expandtab breakindent)
(set! shiftwidth 2 tabstop 2)

; Show tabs for open files on top of window
(set! showtabline 2)

; Enable using the mouse to click around
(set! mouse :a)

; Highlight current line
(set! cursorline)

; Case insensitive searching
(set! ignorecase smartcase)

; Splitting, always below or to the right
(set! splitbelow splitright)

; Required for switching buffers and so on
(set! hidden)

; Backups, swaps and history
(set! backup swapfile)
(set! noundofile)

; How many rows/colums to show around cursor when jumping around
(set! scrolloff 8 sidescrolloff 8)

; Configure themes
(set! termguicolors)

; Set folding to use Tree-Sitter
(set! foldmethod :manual foldexpr "nvim_treesitter#foldexpr()")

(vim.opt.shortmess:append :c)

; bind space as leader key
(do
  (vim.keymap.set [:n :v] :<Space> :<Nop> {:silent true})
  (set vim.g.mapleader " ")
  (set vim.g.maplocalleader " "))
