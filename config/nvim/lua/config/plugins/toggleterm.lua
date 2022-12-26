local M = {
  "akinsho/toggleterm.nvim",
  keys = "<C-t>",
  cmd = { "ToggleTerm", "ToggleTermOpen" },
}

M.config = function()
  require("toggleterm").setup({
    open_mapping = "<c-t>",
    direction = "float",
    shade_terminals = false,
    size = 30,
  })

  function _G.set_terminal_keymaps()
    local opts = { buffer = 0 }
    vim.keymap.set("t", "<esc>", [[<C-\><C-n>]], opts)
    vim.keymap.set("t", "<C-w>h", [[<Cmd>wincmd h<CR>]], opts)
    vim.keymap.set("t", "<C-w>j", [[<Cmd>wincmd j<CR>]], opts)
    vim.keymap.set("t", "<C-w>k", [[<Cmd>wincmd k<CR>]], opts)
    vim.keymap.set("t", "<C-w>l", [[<Cmd>wincmd l<CR>]], opts)
  end

  -- Set window movement in terminal
  vim.cmd("autocmd! TermOpen term://* lua set_terminal_keymaps()")

  -- esc esc to get to normal mode
  vim.cmd([[tnoremap <esc><esc> <C-><C-N>]])
end

return M
