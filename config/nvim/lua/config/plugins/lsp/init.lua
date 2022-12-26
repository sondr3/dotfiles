local M = {
  "neovim/nvim-lspconfig",
  name = "lsp",
  event = "BufReadPre",
  dependencies = {
    "hrsh7th/cmp-nvim-lsp",
    "ray-x/lsp_signature.nvim",
    "j-hui/fidget.nvim",
  },
}

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
M.on_attach = function(client, bufnr)
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

M.signature = function()
  require("lsp_signature").setup({
    doc_lines = 0,
    floating_window = false,
    hint_prefix = " ",
  })
end

M.fidget = function()
  require("fidget").setup({})
end

M.config = function()
  require("mason")
  local lspconfig = require("lspconfig")
  local util = require("lspconfig.util")

  local opts = { noremap = true, silent = true }
  vim.keymap.set("n", "<space>e", "<cmd>lua vim.diagnostic.open_float()<CR>", opts)
  vim.keymap.set("n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
  vim.keymap.set("n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
  vim.keymap.set("n", "<space>q", "<cmd>lua vim.diagnostic.setloclist()<CR>", opts)

  local runtime_path = vim.split(package.path, ";")
  table.insert(runtime_path, "lua/?.lua")
  table.insert(runtime_path, "lua/?/init.lua")

  local capabilities = require("cmp_nvim_lsp").default_capabilities()

  lspconfig.stylelint_lsp.setup({
    on_attach = M.on_attach,
    capabilities = capabilities,
    filetypes = {
      "css",
      "less",
      "scss",
      "sugarss",
      "vue",
      "wxss",
      "javascriptreact",
      "typescriptreact",
    },
    settings = {
      stylelintplus = {
        cssInJs = false,
      },
    },
  })
  lspconfig.tsserver.setup({
    on_attach = M.on_attach,
    capabilities = capabilities,
    root_dir = util.root_pattern("package.json", "tsconfig.json"),
    init_options = {
      lint = true,
    },
  })
  lspconfig.denols.setup({
    on_attach = M.on_attach,
    capabilities = capabilities,
    root_dir = util.root_pattern("deno.json", "deno.jsonc", "import_map.json"),
    single_file_support = false,
    init_options = {
      enable = true,
      lint = true,
      unstable = true,
    },
  })

  require("rust-tools").setup({
    server = {
      on_attach = M.on_attach,
      capabilities = capabilities,
      settings = {
        ["rust-analyzer"] = {
          checkOnSave = { command = "clippy" },
        },
      },
    },
  })

  local ht = require("haskell-tools")
  ht.setup({
    hls = {
      on_attach = function(client, bufnr)
        local local_opts = vim.tbl_extend("keep", opts, { buffer = bufnr })

        -- haskell-language-server relies heavily on codeLenses,
        -- so auto-refresh (see advanced configuration) is enabled by default
        vim.keymap.set("n", "<space>hs", ht.hoogle.hoogle_signature, local_opts)

        -- Toggle a GHCi repl for the current package
        vim.keymap.set("n", "<leader>rr", ht.repl.toggle, opts)
        -- Toggle a GHCi repl for the current buffer
        vim.keymap.set("n", "<leader>rf", function()
          ht.repl.toggle(vim.api.nvim_buf_get_name(0))
        end, opts)
        vim.keymap.set("n", "<leader>rq", ht.repl.quit, opts)
        M.on_attach(client, bufnr)
      end,
      settings = {
        haskell = {
          formattingProvider = "ormolu",
          checkProject = true,
        },
      },
    },
  })

  lspconfig.sumneko_lua.setup({
    on_attach = M.on_attach,
    capabilities = capabilities,
    settings = {
      Lua = {
        runtime = { version = "LuaJIT", path = runtime_path },
        diagnostics = { globals = { "vim" } },
        workspace = {
          library = vim.api.nvim_get_runtime_file("", true),
          checkThirdParty = false,
          maxPreload = 10000,
          preloadFileSize = 50000,
        },
        telemetry = { enable = false },
      },
    },
  })

  lspconfig.tailwindcss.setup({
    on_attach = M.on_attach,
    capabilities = capabilities,
    root_dir = util.root_pattern(
      "tailwind.config.js",
      "tailwind.config.cjs",
      "postcss.config.js",
      "postcss.config.cjs"
    ),
  })

  lspconfig.svelte.setup({
    on_attach = M.on_attach,
    capabilities = capabilities,
    root_dir = util.root_pattern("svelte.config.cjs", "svelte.config.js"),
  })

  for _, server in ipairs({
    "cssls",
    --[[ "dhall_lsp_server", ]]
    "astro",
    "html",
    "jsonls",
  }) do
    lspconfig[server].setup({ on_attach = M.on_attach, capabilities = capabilities })
  end

  M.signature()
  M.fidget()
end

return M
