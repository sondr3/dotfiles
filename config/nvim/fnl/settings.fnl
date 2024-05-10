(import-macros {: set! } :setting-macros)

; UTF8 or bust
(set! fileencoding "utf-8")

; Incremental live completion
(set! inccommand "split")

; Use system clipboard
(set! clipboard "unnamedplus")

; Hide the `-- INSERT --` messages
(set! noshowmode)

; Highlight searches when done searching
(set! hlsearch)

; Enable relative line numbers
(set! number relativenumber)

; Increase width of number columns and always show sign column
(set! numberwidth 4 
      signcolumn "yes")

; For faster completion
(set! updatetime 250 
      timeoutlen 300
      completeopt ["menu" "menuone" "noselect"])

; Tabs vs spaces, make indenting great again
(set! smartindent expandtab breakindent)
(set! shiftwidth 2 tabstop 2)

; Show tabs for open files on top of window
(set! showtabline 2)

; Enable using the mouse to click around
(set! mouse "a")

; Highlight current line
(set! cursorline)

; Case insensitive searching
(set! ignorecase smartcase)

; Splitting, always below or to the right
(set! splitbelow splitright)

; Required for switching buffers and so on
(set! hidden)

; Backups, swaps and history
(set! nobackup noswapfile undofile)

; How many rows/colums to show around cursor when jumping around
(set! scrolloff 8 sidescrolloff 8)

; Configure themes
(set! termguicolors)

; Sets how neovim will display certain whitespace characters in the editor.
(set! list
      listchars {:nbsp "␣" :tab "» " :trail "·"})

; Set folding to use Tree-Sitter
(set! foldmethod :manual
      foldexpr "nvim_treesitter#foldexpr()")
