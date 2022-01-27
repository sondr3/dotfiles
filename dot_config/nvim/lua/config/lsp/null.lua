local null = require("null-ls")
local builtins = null.builtins

null.setup({
  sources = {
    builtins.formatting.stylua,
    builtins.formatting.prettierd,
    builtins.formatting.eslint_d.with({ extra_args = { "--cache" } }),
    builtins.diagnostics.eslint_d.with({ extra_args = { "--cache" } }),
    builtins.formatting.cabal_fmt,
  },
  on_attach = function(client)
    if client.resolved_capabilities.document_formatting then
      vim.cmd("autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()")
    end
  end,
})
