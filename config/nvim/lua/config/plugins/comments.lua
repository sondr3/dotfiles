local M = {
  "numToStr/Comment.nvim",
  keys = { "gc", "gcc", "gbc" },
  dependencies = {
    "JoosepAlviste/nvim-ts-context-commentstring",
  },
}

M.config = function()
  require("nvim-treesitter.configs").setup({
    context_commentstring = {
      enable = true,
      enable_autocmd = false,
    },
  })
  require("Comment").setup({
    pre_hook = require("ts_context_commentstring.integrations.comment_nvim").create_pre_hook(),
  })
end

return M
