local M = {
  "folke/trouble.nvim",
  dependencies = "nvim-tree/nvim-web-devicons",
  cmd = { "TroubleToggle", "Trouble" },
}

M.config = function()
  require("trouble").setup({
    auto_open = false,
    use_diagnostic_signs = true, -- en
  })
end

return M
