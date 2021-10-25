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
