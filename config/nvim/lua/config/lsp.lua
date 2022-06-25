local lsp_servers = { "rust_analyzer", "sumneko_lua", "tsserver", "denols", "tsserver" }

require("nvim-lsp-installer").setup({
  ensure_installed = lsp_servers,
  automatic_installation = true,
  ui = {
    icons = {
      server_installed = "✓",
      server_pending = "➜",
      server_uninstalled = "✗",
    },
  },
})

local lspconfig = require("lspconfig")
local util = require("lspconfig.util")
local null = require("null-ls")
local builtins = null.builtins

local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<space>e", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
vim.keymap.set("n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
vim.keymap.set("n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
vim.keymap.set("n", "<space>q", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  local attach_opts = { silent = true, buffer = bufnr }

  vim.keymap.set("n", "gh", vim.lsp.buf.hover, attach_opts)
  vim.keymap.set("n", "gD", vim.lsp.buf.declaration, attach_opts)
  vim.keymap.set("n", "gd", require("telescope.builtin").lsp_definitions, attach_opts)
  vim.keymap.set("n", "gr", require("telescope.builtin").lsp_references, attach_opts)
  vim.keymap.set("n", "gS", require("telescope.builtin").lsp_workspace_symbols, attach_opts)
  vim.keymap.set("n", "gs", require("telescope.builtin").lsp_document_symbols, attach_opts)
  vim.keymap.set("n", "gF", function()
    require("telescope.builtin").lsp_workspace_symbols({ symbols = "functions" })
  end, attach_opts)
  vim.keymap.set("n", "gf", function()
    require("telescope.builtin").lsp_document_symbols({ symbols = "functions" })
  end, attach_opts)
  vim.keymap.set("n", "gi", require("telescope.builtin").lsp_implementations, attach_opts)
  vim.keymap.set("n", "gt", require("telescope.builtin").lsp_type_definitions, attach_opts)
  vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, attach_opts)
  vim.keymap.set("n", "<space>Wa", vim.lsp.buf.add_workspace_folder, attach_opts)
  vim.keymap.set("n", "<space>Wr", vim.lsp.buf.remove_workspace_folder, attach_opts)
  vim.keymap.set("n", "<space>Wl", function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, attach_opts)
  vim.keymap.set("n", "<space>rn", vim.lsp.buf.rename, attach_opts)
  vim.keymap.set("n", "<space>ca", vim.lsp.buf.code_action, attach_opts)
  vim.keymap.set("n", "<space>cA", vim.lsp.buf.range_code_action, attach_opts)
  vim.keymap.set("n", "<space>f", vim.lsp.buf.formatting, attach_opts)

  -- trouble.nvim
  vim.keymap.set("n", "<space>xx", function()
    require("trouble").toggle()
  end, attach_opts)
  vim.keymap.set("n", "<space>xw", function()
    require("trouble").toggle({ mode = "workspace_diagnostics" })
  end, attach_opts)
  vim.keymap.set("n", "<space>xd", function()
    require("trouble").toggle({ mode = "document_diagnostics" })
  end, attach_opts)
  vim.keymap.set("n", "<space>xl", function()
    require("trouble").toggle({ mode = "loclist" })
  end, attach_opts)
  vim.keymap.set("n", "<space>xq", function()
    require("trouble").toggle({ mode = "quickfix" })
  end, attach_opts)
  vim.keymap.set("n", "<space>xr", function()
    require("trouble").toggle({ mode = "lsp_references" })
  end, attach_opts)

  if client.server_capabilities.documentHighlight then
    -- highlight on hover
    local group = vim.api.nvim_create_augroup("lsp_document_highlight", { clear = true })
    vim.api.nvim_create_autocmd("CursorHold", {
      callback = vim.lsp.buf.document_highlight,
      group = group,
    })
    vim.api.nvim_create_autocmd("CursorMoved", {
      callback = vim.lsp.buf.clear_references,
      group = group,
    })
  end

  -- setup formatting
  local ok, lsp_format = pcall(require, "lsp-format")
  if ok then
    lsp_format.on_attach(client)
  end
end

null.setup({
  sources = {
    -- formatting
    builtins.formatting.stylua,
    builtins.formatting.prettierd,
    builtins.formatting.cabal_fmt,
    builtins.formatting.rustfmt,
    builtins.formatting.fnlfmt,
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

    on_attach(client, bufnr)
  end,
})

local runtime_path = vim.split(package.path, ";")
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

lspconfig.stylelint_lsp.setup({
  on_attach = on_attach,
  filetypes = { "css", "less", "scss", "sugarss", "vue", "wxss", "javascriptreact", "typescriptreact" },
  settings = {
    stylelintplus = {
      cssInJs = false,
    },
  },
})
lspconfig.tsserver.setup({
  on_attach = on_attach,
  root_dir = util.root_pattern("package.json"),
  init_options = {
    lint = true,
  },
})
lspconfig.denols.setup({
  on_attach = on_attach,
  root_dir = util.root_pattern("deno.json"),
  init_options = {
    enable = true,
    lint = true,
    unstable = true,
    importMap = "./import_map.json",
    config = "./deno.json",
  },
})

require("rust-tools").setup({
  server = {
    on_attach = on_attach,
    settings = {
      ["rust-analyzer"] = {
        checkOnSave = { command = "clippy" },
      },
    },
  },
})
lspconfig.sumneko_lua.setup({
  on_attach = on_attach,
  settings = {
    Lua = {
      runtime = { version = "LuaJIT", path = runtime_path },
      diagnostics = { globals = { "vim" } },
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true),
      },
    },
  },
})

lspconfig.ltex.setup({
  settings = {
    ltex = {
      checkFrequency = "save",
    },
  },
})

lspconfig.purescriptls.setup({
  on_attach = on_attach,
  settings = {
    purescript = {
      addSpagoSources = true,
      formatter = "purs-tidy",
    },
  },
})

for _, server in ipairs({
  "cssls",
  "dhall_lsp_server",
  "html",
  "hls",
  "jsonls",
  "svelte",
  "tailwindcss",
  "taplo",
  "texlab",
}) do
  lspconfig[server].setup({ on_attach = on_attach })
end
