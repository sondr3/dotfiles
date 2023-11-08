-- Install lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.notify("Bootstrapping lazy.nvim...", vim.log.levels.INFO)
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end

-- Install hotpot.nvim for fennel
local hotpotpath = vim.fn.stdpath("data") .. "/lazy/hotpot.nvim"
if not vim.loop.fs_stat(hotpotpath) then
  vim.notify("Bootstrapping hotpot.nvim...", vim.log.levels.INFO)
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "--single-branch",
    "--branch=v0.9.7",
    "https://github.com/rktjmp/hotpot.nvim.git",
    hotpotpath,
  })
end

vim.opt.runtimepath:prepend({ hotpotpath, lazypath })
vim.loader.enable()

-- Configure hotpot
require("hotpot").setup({
  provide_require_fennel = true,
  enable_hotpot_diagnostics = true,
  compiler = {
    modules = {
      correlate = true,
    },
    macros = {
      env = "_COMPILER",
      compilerEnv = _G,
      allowedGlobals = false,
    }
  },
})

-- Set initial configuration for nvim
require("settings")

local packages = { "rktjmp/hotpot.nvim" }
-- local packages_folder = vim.fn.stdpath("config") .. "/fnl/packages"
-- if vim.loop.fs_stat(packages_folder) then
--   for file in vim.fs.dir(packages_folder) do
--     file = file:match("^(.*)%.fnl$")
--     table.insert(packages, require("packages." .. file))
--   end
-- end

require("lazy").setup(packages, {
  defaults = { lazy = true },
  install = { colorscheme = { "nordic" } },
  checker = { enabled = true },
})

vim.api.nvim_create_autocmd("User", {
  pattern = "VeryLazy",
  callback = function()
    -- Load mappings
    require("config.mappings")
    -- require('config.events')
    -- require('config.commands')
  end,
})
