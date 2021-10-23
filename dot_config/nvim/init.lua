local execute = vim.api.nvim_command
local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost init.lua | PackerCompile
  augroup end
]])

require("packer").startup(function(use)
  use 'wbthomason/packer.nvim'

  use {
    'nvim-telescope/telescope.nvim',
    requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
  }

  use 'shaunsingh/nord.nvim'

  use {
    "nvim-treesitter/nvim-treesitter",
    branch = "0.5-compat",
    run = ":TSUpdate"
  }

  use "neovim/nvim-lspconfig"
  use "hrsh7th/nvim-compe"
  use "simrat39/rust-tools.nvim"

  if packer_bootstrap then
    require('packer').sync()
  end
end)

local default_settings = {
  fileencoding = "utf-8",

  -- Incremental live completion, default on `master`
  inccommand = "nosplit",

  -- Use system clipboard
  clipboard = "unnamedplus",

  -- Hide the `-- INSERT --` messages
  showmode = false,
  
  -- Highlight searches when done searching
  hlsearch = true,
  
  -- Enable relative line numbers
  number = true,
  relativenumber = true,

  -- Increase width of number columns and always show sign column
  numberwidth = 4,
  signcolumn = "yes",

  -- For faster completion
  updatetime = 250,
  timeoutlen = 100,
  
  completeopt = { "menuone", "noselect" },
  
  -- Tabs vs spaces, make indenting great again
  smartindent = true,
  expandtab = true,
  breakindent = true,
  shiftwidth = 2,
  tabstop = 2,
  
  -- Show tabs for open files on top of window
  showtabline = 2,
  
  -- Enable using the mouse to click around
  mouse = "a",

  -- Highlight current line
  cursorline = true,

  -- Case insensitive searching
  ignorecase = true,
  smartcase = true,

  -- Splitting, always below or to the right
  splitbelow = true,
  splitright = true,

  -- Required for switching buffers and so on
  hidden = true,

  -- Backups, swaps and history
  backup = false,
  swapfile = false,
  undofile = true,

  -- How many rows/colums to show around cursor when jumping around
  scrolloff = 8,
  sidescrolloff = 8,

  -- Configure themes
  termguicolors = true,

  -- Set folding to use Tree-Sitter
  foldmethod = "manual",
  foldexpr = "nvim_treesitter#foldexpr()"
}

for k, v in pairs(default_settings) do
  vim.opt[k] = v
end

-- Set theme
vim.cmd[[colorscheme nord]]


require("nvim-treesitter.configs").setup {
  ensure_installed = "maintained",
  highlight = { enable = true }
}

vim.api.nvim_set_keymap('', '<Space>', '<Nop>', { noremap = true, silent=true})
vim.g.mapleader = [[ ]]
vim.g.maplocalleader = [[,]]

local nvim_lsp = require("lspconfig")
local on_attach = function(_client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
end

local lsp_servers = { "rust_analyzer" }
for _, lsp in ipairs(lsp_servers) do
  nvim_lsp[lsp].setup { on_attach = on_attach }
end

require("lspconfig").rust_analyzer.setup {}
require("rust-tools").setup {}

require("compe").setup {
  enabled = true;
  autocomplete = true;

  source = {
    path = true;
    nvim_lsp = true;
  };
}

local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
    local col = vim.fn.col('.') - 1
    if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
        return true
    else
        return false
    end
end

-- Use (s-)tab to:
--- move to prev/next item in completion menuone
--- jump to prev/next snippet's placeholder
_G.tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-n>"
  elseif check_back_space() then
    return t "<Tab>"
  else
    return vim.fn['compe#complete']()
  end
end
_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-p>"
  else
    -- If <S-Tab> is not working in your terminal, change it to <C-h>
    return t "<S-Tab>"
  end
end

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})

vim.api.nvim_set_keymap("i", "<C-space>", "compe#complete()", { silent = true, noremap = true, expr = true })
vim.api.nvim_set_keymap("i", "<CR>", "compe#confirm('<CR>')", { silent = true, noremap = true, expr = true })
vim.api.nvim_set_keymap("i", "<C-e>", "compe#close('<C-e>')", { silent = true, noremap = true, expr = true })
