local M = {
  "jose-elias-alvarez/null-ls.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "lukas-reineke/lsp-format.nvim",
  },
}

local eslint_options = {
  extra_args = { "--cache" },
  condition = function(utils)
    return utils.root_has_file({ "package.json", ".eslintrc.js", ".eslintrc.cjs" })
  end,
}

M.config = function()
  local null = require("null-ls")
  local builtins = null.builtins
  local lsp = require("config.plugins.lsp")

  require("lsp-format").setup({ sync = true })
  null.setup({
    sources = {
      -- formatting
      builtins.formatting.stylua,
      builtins.formatting.deno_fmt.with({
        condition = function(utils)
          return utils.root_has_file({ "deno.json", "deno.jsonc", "import_map.json" })
        end,
      }),
      builtins.formatting.prettier.with({
        condition = function(utils)
          return utils.root_has_file({ "package.json", ".prettierrc", ".prettierrc.js" })
        end,
      }),
      builtins.formatting.cabal_fmt,
      builtins.formatting.rustfmt,
      builtins.formatting.eslint.with(eslint_options),

      -- diagnostics
      builtins.diagnostics.eslint.with(eslint_options),

      -- code actions
      builtins.code_actions.eslint.with(eslint_options),
    },
    on_attach = function(client, bufnr)
      local ok, lsp_format = pcall(require, "lsp-format")
      if ok then
        lsp_format.on_attach(client)
      end

      lsp.on_attach(client, bufnr)
    end,
  })
end

return M
