local util = require("lspconfig.util")

local lsp = require("config/lsp")

local lsp_servers = { "rust_analyzer", "sumneko_lua", "tsserver", "denols" }

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

local null_ls_formatting = function(client)
  client.resolved_capabilities.document_formatting = false
  client.resolved_capabilities.document_range_formatting = false
end

local runtime_path = vim.split(package.path, ";")
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

local servers = {
  stylelint_lsp = {
    filetypes = { "css", "less", "scss", "sugarss", "vue", "wxss", "javascriptreact", "typescriptreact" },
    on_attach = function(client, bufnr)
      null_ls_formatting(client)
      lsp.on_attach(client, bufnr)
    end,
    settings = {
      stylelintplus = {
        cssInJs = false,
      },
    },
  },
  jsonls = {
    on_attach = function(client, bufnr)
      null_ls_formatting(client)
      lsp.on_attach(client, bufnr)
    end,
  },
  tsserver = {
    root_dir = util.root_pattern("package.json"),
    on_attach = function(client, bufnr)
      null_ls_formatting(client)
      lsp.on_attach(client, bufnr)
    end,
    init_options = {
      lint = true,
    },
  },
  denols = {
    root_dir = util.root_pattern("deno.json"),
    init_options = {
      enable = true,
      lint = true,
      unstable = true,
      importMap = "./import_map.json",
      config = "./deno.json",
    },
  },
  rust_analyzer = {
    settings = {
      ["rust-analyzer"] = {
        checkOnSave = { command = "clippy" },
      },
    },
  },
  sumneko_lua = {
    on_attach = function(client, bufnr)
      null_ls_formatting(client)
      lsp.on_attach(client, bufnr)
    end,
    settings = {
      Lua = {
        runtime = { version = "LuaJIT", path = runtime_path },
        diagnostics = { globals = { "vim" } },
        workspace = {
          library = vim.api.nvim_get_runtime_file("", true),
        },
      },
    },
  },
}

local server_opts = function(server)
  if servers[server.name] ~= nil then
    return vim.tbl_deep_extend("keep", servers[server.name], lsp)
  else
    return lsp
  end
end

lsp_installer.on_server_ready(function(server)
  local opts = server_opts(server)
  opts.capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities())

  if server.name == "rust_analyzer" then
    require("rust-tools").setup({
      server = vim.tbl_deep_extend("force", server:get_default_options(), opts),
    })
    server:attach_buffers()
  else
    server:setup(opts)
  end
end)
