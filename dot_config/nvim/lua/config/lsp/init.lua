local opts = { noremap = true, silent = true }
vim.api.nvim_set_keymap("n", "<space>e", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
vim.api.nvim_set_keymap("n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
vim.api.nvim_set_keymap("n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
vim.api.nvim_set_keymap("n", "<space>q", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(_, bufnr)
  local function buf_set_keymap(key, cmd)
    vim.api.nvim_buf_set_keymap(bufnr, "n", key, cmd, opts)
  end

  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap("gh", "<cmd>lua vim.lsp.buf.hover()<CR>")
  buf_set_keymap("gD", "<cmd>lua vim.lsp.buf.declaration()<CR>")
  buf_set_keymap("gd", "<cmd>lua require('telescope.builtin').lsp_definitions()<CR>")
  buf_set_keymap("gr", "<cmd>lua require('telescope.builtin').lsp_references()<CR>")
  buf_set_keymap("gS", "<cmd>lua require('telescope.builtin').lsp_workspace_symbols()<CR>")
  buf_set_keymap("gs", "<cmd>lua require('telescope.builtin').lsp_document_symbols()<CR>")
  buf_set_keymap("gF", "<cmd>lua require('telescope.builtin').lsp_workspace_symbols({symbols='functions'})<CR>")
  buf_set_keymap("gf", "<cmd>lua require('telescope.builtin').lsp_document_symbols({symbols='functions'})<CR>")
  buf_set_keymap("gi", "<cmd>lua require('telescope.builtin').lsp_implementations()<CR>")
  buf_set_keymap("gt", "<cmd>lua require('telescope.builtin').lsp_type_definition()<CR>")
  buf_set_keymap("<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>")
  buf_set_keymap("<space>Wa", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>")
  buf_set_keymap("<space>Wr", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>")
  buf_set_keymap("<space>Wl", "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>")
  buf_set_keymap("<space>rn", "<cmd>lua vim.lsp.buf.rename()<CR>")
  buf_set_keymap("<space>ca", "<cmd>lua require('telescope.builtin').lsp_code_actions()<CR>")
  buf_set_keymap("<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>")

  -- trouble.nvim
  buf_set_keymap("<space>xx", "<cmd>Trouble<CR>")
  buf_set_keymap("<space>xw", "<cmd>Trouble lsp_workspace_diagnostics<CR>")
  buf_set_keymap("<space>xd", "<cmd>Trouble lsp_document_diagnostics<CR>")
  buf_set_keymap("<space>xl", "<cmd>Trouble loclist<CR>")
  buf_set_keymap("<space>xq", "<cmd>Trouble quickfix<CR>")
  buf_set_keymap("gR", "<cmd>Trouble lsp_references<CR>")

  -- format document
  vim.cmd([[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()]])
end

local flags = {
  debounce_text_changes = 150,
}

return {
  on_attach = on_attach,
  flags = flags,
}
