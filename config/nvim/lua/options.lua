-- set space to the leader key
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- disable the default behaviour of <Space> in Normal and Visual mode
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })

-- utf8 baby
vim.opt.fileencoding = "utf-8"

-- Enable relative line numbers
vim.o.number = true
vim.o.relativenumber = true

-- sync clipboard between nvim and OS
vim.schedule(function() vim.o.clipboard = "unnamedplus" end)

-- Increase width of number columns and always show sign column
vim.o.numberwidth = 4
vim.o.signcolumn = "yes"

-- Enable using the mouse to click around
vim.o.mouse = "a"

-- Don't show the mode, since it's already in the status line
vim.o.showmode = false

-- Highlight current line
vim.o.cursorline = true

-- Case insensitive searching
vim.o.ignorecase = true
vim.o.smartcase = true

-- Splitting, always below or to the right
vim.o.splitbelow = true
vim.o.splitright = true

-- Required for switching buffers and so on
vim.o.hidden = true

-- Tabs vs spaces, make indenting great again
vim.o.smartindent = true
vim.o.expandtab = true
vim.o.breakindent = true
vim.o.shiftwidth = 2
vim.o.tabstop = 2

-- Faster update time
vim.o.updatetime = 250

-- Faster mapped sequence wait time
vim.o.timeoutlen = 300

-- Backups, swaps and history
vim.o.backup = false
vim.o.swapfile = false
vim.o.undofile = true

-- How many rows/colums to show around cursor when jumping around
vim.o.scrolloff = 8
vim.o.sidescrolloff = 8

-- Configure themes
vim.o.termguicolors = true

-- Show tabs for open files on top of window
vim.o.showtabline = 2

-- Sets how neovim will display certain whitespace characters in the editor.
vim.o.list = true
vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }

-- Preview substitutions live, as you type!
vim.o.inccommand = "split"

-- show confirmation on destructive things
vim.o.confirm = true

-- Disable line wrapping
vim.o.wrap = false

-- disable the cmdline
vim.o.cmdheight = 0

-- fake netrw being used for yazi
vim.g.loaded_netrwPlugin = 1
vim.g.loaded_netrw = 1
