local M = {
  "L3MON4D3/LuaSnip",
  dependencies = {

    "rafamadriz/friendly-snippets",
    config = function()
      require("luasnip.loaders.from_vscode").lazy_load()
    end,
  },
}

M.config = function()
  require("luasnip").config.setup({
    history = false,
    updateevents = "TextChanged,TextChangedI",
    enable_autosnippets = true,
  })
end

return M
