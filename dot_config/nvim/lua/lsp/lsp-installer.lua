local lsp = require("lsp")

local lsp_servers = { "rust_analyzer", "sumneko_lua", "tsserver" }

for _, name in ipairs(lsp_servers) do
  local ok, server = require("nvim-lsp-installer.servers").get_server(name)

  if ok and not server:is_installed() then
    print("Installing LSP server " .. name)
    server:install()
  end
end

local lsp_installer = require("nvim-lsp-installer")

lsp_installer.settings({
  ui = {
    icons = {
      server_installed = "✓",
      server_pending = "➜",
      server_uninstalled = "✗",
    },
  },
})

lsp_installer.on_server_ready(function(server)
  local opts = {}
  opts.capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities())
  opts.on_attach = lsp.on_attach

  if server.name == "sumneko_lua" then
    opts.settings = {
      Lua = {
        runtime = { version = "LuaJIT", path = vim.split(package.path, ";") },
        diagnostics = { globals = { "vim" } },
        workspace = {
          library = {
            [vim.fn.expand("$VIMRUNTIME/lua")] = true,
            [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
          },
        },
      },
    }
  elseif server.name == "rust_analyzer" then
    local _, req_server = require("nvim-lsp-installer.servers").get_server(server.name)
    opts.server = {
      cmd = req_server._default_options.cmd,
      on_attach = opts.on_attach,
    }
  end

  if server.name == "rust_analyzer" then
    require("rust-tools").setup(opts)
  else
    server:setup(opts)
  end

  vim.cmd([[ do User LspAttachBuffers ]])
end)
