-- Speed up loading Nvim files
require("impatient")
local comp, _ = pcall(require, "packer_compiled")
if not comp then
  print("You are missing packer_compiled.lua, run :PackerCompile to fix")
end

require("plugins")
require("settings")

require("statusline")
