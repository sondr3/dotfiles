-- Use Neovim file type detection
vim.g.do_filetype_lua = 1
vim.g.did_load_filetypes = 0

-- Speed up loading Nvim files
local impatient_ok, _ = pcall(require, "impatient")
if not impatient_ok then
  print("impatient.nvim missing")
end

-- Compiled stuffs
local comp, _ = pcall(require, "packer_compiled")
if not comp then
  print("You are missing packer_compiled.lua, run :PackerCompile to fix")
end

require("plugins")
require("settings")

require("statusline")
