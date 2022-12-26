-- Set initial configuration for nvim
require("config.options")

-- Setup packages
require("config.lazy")


vim.api.nvim_create_autocmd("User", {
  pattern = "VeryLazy",
  callback = function()
    -- Load mappings
    require('config.mappings')
    -- require('config.events')
    -- require('config.commands')
  end
})
