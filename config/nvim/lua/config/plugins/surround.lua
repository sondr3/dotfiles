local M = {
  "kylechui/nvim-surround",
  event = "BufReadPre",
  config = function()
    require("nvim-surround").setup({})
  end,
}

return M
