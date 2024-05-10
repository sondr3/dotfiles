-- bootstrap tangerine.nvim for Fennel
local function bootstrap(url)
  local name = url:gsub(".*/", "")
  local path

  path = vim.fn.stdpath("data") .. "/lazy/" .. name
  vim.opt.rtp:prepend(path)

  if vim.fn.isdirectory(path) == 0 then
    print(name .. ": installing in data dir...")

    vim.fn.system({ "git", "clone", url, path })
    vim.cmd("redraw")
    print(name .. ": finished installing")
  end
end

bootstrap("https://github.com/udayvir-singh/tangerine.nvim")

vim.loader.enable()

require("tangerine").setup({
  compiler = {
    version = "1-4-2",
    verbose = false,
    hooks = { "onsave", "oninit" },
  },
  keymaps = {},
})

-- Set initial configuration for nvim
-- require("config.options")

-- Setup packages
-- require("config.lazy")

-- vim.api.nvim_create_autocmd("User", {
--   pattern = "VeryLazy",
--   callback = function()
--     -- Load mappings
--     require("config.mappings")
--     -- require('config.events')
--     -- require('config.commands')
--   end,
-- })
