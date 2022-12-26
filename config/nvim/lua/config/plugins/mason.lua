local M = {
  "williamboman/mason.nvim",
  dependencies = {
    "williamboman/mason-lspconfig.nvim",
    "WhoIsSethDaniel/mason-tool-installer.nvim",
  },
  cmd = "Mason",
}

M.config = function()
  require("mason").setup({
    providers = {
      "mason.providers.client",
      "mason.providers.registry-api",
    },
    ui = {
      icons = {
        server_installed = "✓",
        server_pending = "➜",
        server_uninstalled = "✗",
      },
    },
  })

  require("mason-lspconfig").setup({
    automatic_installation = true,
  })
end

return M
