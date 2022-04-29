local null = require("null-ls")
local builtins = null.builtins

local lsp = require("config.lsp")

null.setup({
  sources = {
    -- formatting
    builtins.formatting.stylua,
    builtins.formatting.prettierd,
    builtins.formatting.cabal_fmt,
    builtins.formatting.rustfmt,
    builtins.formatting.eslint_d.with({ extra_args = { "--cache" } }),

    -- diagnostics
    builtins.diagnostics.eslint_d.with({ extra_args = { "--cache" } }),

    -- code actions
    builtins.code_actions.eslint_d.with({ extra_args = { "--cache" } }),
  },
  on_attach = function(client, bufnr)
    local ok, lsp_format = pcall(require, "lsp-format")
    if ok then
      lsp_format.on_attach(client)
    end

    lsp.on_attach(client, bufnr)
  end,
})
