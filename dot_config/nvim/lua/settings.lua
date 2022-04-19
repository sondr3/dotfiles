local default_settings = {
  fileencoding = "utf-8",

  -- Incremental live completion, default on `master`
  inccommand = "nosplit",

  -- Use system clipboard
  clipboard = vim.opt.clipboard + "unnamedplus",

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
  timeoutlen = 500,

  completeopt = { "menu", "menuone", "noselect" },

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
  foldexpr = "nvim_treesitter#foldexpr()",
}

for k, v in pairs(default_settings) do
  vim.opt[k] = v
end

vim.opt.shortmess:append("c")

-- Show diagnostics on hover instead of as virtual text
local hover_group = vim.api.nvim_create_augroup("on_hover", { clear = true })
vim.api.nvim_create_autocmd("CursorHold", {
  pattern = "*",
  group = hover_group,
  callback = function()
    vim.diagnostic.open_float(0, { scope = "cursor", focus = false })
  end,
})

-- Highlight on yank
local highlight_group = vim.api.nvim_create_augroup("yank_highlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
  pattern = "*",
  group = highlight_group,
  callback = function()
    vim.highlight.on_yank()
  end,
})

vim.diagnostic.config({
  virtual_text = true,
  signs = true,
  float = { border = "single" },
})
