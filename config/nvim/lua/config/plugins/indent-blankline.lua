local M = {
  "lukas-reineke/indent-blankline.nvim",
  event = "BufReadPre"
}

M.config = function()
  require("indent_blankline").setup({
    buftype_exclude = { "terminal", "nofile" },
    filetype_exclude = { "help", "packer" },
    use_treesitter = true,
    show_trailing_blankline_indent = false,
  })
end

return M
