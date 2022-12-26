local M = {
  "lewis6991/gitsigns.nvim",
  dependencies = { "nvim-lua/plenary.nvim" },
}

M.config = function()
  require("gitsigns").setup({
    signs = {
      add = { hl = "GitGutterAdd", text = "+" },
      change = { hl = "GitGutterChange", text = "~" },
      delete = { hl = "GitGutterDelete", text = "_" },
      topdelete = { hl = "GitGutterDelete", text = "‾" },
      changedelete = { hl = "GitGutterChange", text = "~" },
    },
  })
end

return M
