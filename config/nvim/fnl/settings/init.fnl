(import-macros {: set! : set-true! : set-false!} :settings.macros)

; always UTF8
(set! fileencoding :utf-8)
; Incremental live completion, default on `master`
(set! inccommand :nosplit)
; Use system clipboard
(set! clipboard (+ vim.opt.clipboard :unnamedplus))

; Highlight searches when done searching
(set-true! hlsearch)

; Hide the `-- INSERT --` messages
(set-false! showmode)

;; enable relative line numbers
(set-true! number relativenumber)

; Increase width of number columns and always show sign column
(set! numberwidth 4 signcolumn :yes)

; For faster completion
(set! updatetime 250 timeoutlen 500)

; Set completeopt to have a better completion experience
(set! completeopt [:menu :menuone :noselect])

; Tabs vs spaces, make indenting great again
(set-true! smartindent expandtab breakindent)
(set! shiftwidth 2 tabstop 2)

; Show tabs for open files on top of window
(set! showtabline 2)

; Enable using the mouse to click around
(set! mouse :a)

; Highlight current line
(set-true! cursorline)

; Case insensitive searching
(set-true! ignorecase smartcase)

; Splitting, always below or to the right
(set-true! splitbelow splitright)

; Required for switching buffers and so on
(set-true! hidden)

; Backups, swaps and history
(set-true! backup swapfile)
(set-false! undofile)

; How many rows/colums to show around cursor when jumping around
(set! scrolloff 8 sidescrolloff 8)

; Configure themes
(set-true! termguicolors)

; Set folding to use Tree-Sitter
(set! foldmethod :manual foldexpr "nvim_treesitter#foldexpr()")

(vim.opt.shortmess:append :c)
