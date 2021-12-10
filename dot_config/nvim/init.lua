-- Speed up loading Nvim files
require("impatient")
require("packer_compiled")

require("plugins")

require("settings")
require("statusline")

vim.api.nvim_set_keymap("", "<Space>", "<Nop>", { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = " "

local actions = require("telescope.actions")
local trouble = require("trouble.providers.telescope")

require("telescope").setup({
  defaults = {
    initial_mode = "insert",
    mappings = {
      i = {
        ["<esc>"] = actions.close, -- escape closes popup
        ["<c-t>"] = trouble.open_with_trouble,
      },
      n = {
        ["<c-t>"] = trouble.open_with_trouble,
      },
    },
  },
  extensions = {
    fzf = {
      fuzzy = true, -- false will only do exact matching
      override_generic_sorter = true, -- override the generic sorter
      override_file_sorter = true, -- override the file sorter
      case_mode = "smart_case", -- or "ignore_case" or "respect_case", the default case_mode is "smart_case"
    },
  },
})

require("telescope").load_extension("fzf")

local lua_leader_map = require("utils").lua_leader_map
local leader_map = require("utils").leader_map

lua_leader_map("<space>", "require('telescope.builtin').buffers()")
lua_leader_map("bb", "require('telescope.builtin').buffers()")

lua_leader_map("ff", "require('telescope.builtin').find_files()")
lua_leader_map("fb", "require('telescope.builtin').file_browser()")
lua_leader_map("fg", "require('telescope.builtin').live_grep()")
lua_leader_map("fs", "require('telescope.builtin').grep_string()")

lua_leader_map("gs", "require('telescope.builtin').git_status()")
lua_leader_map("gb", "require('telescope.builtin').git_branches()")
lua_leader_map("gc", "require('telescope.builtin').git_commits()")

leader_map("t", "ToggleTerm")
leader_map("T", "ToggleTerm direction='float'")

require("gitsigns").setup({
  signs = {
    add = { hl = "GitGutterAdd", text = "+" },
    change = { hl = "GitGutterChange", text = "~" },
    delete = { hl = "GitGutterDelete", text = "_" },
    topdelete = { hl = "GitGutterDelete", text = "‾" },
    changedelete = { hl = "GitGutterChange", text = "~" },
  },
})

require("indent_blankline").setup({
  buftype_exclude = { "terminal", "nofile" },
  filetype_exclude = { "help", "packer" },
  use_treesitter = true,
  show_trailing_blankline_indent = false,
})

require("toggleterm").setup({
  open_mapping = [[<c-t>]],
  size = 30,
})

require("nordic").colorscheme({})

require("nvim-autopairs").setup({
  check_ts = true,
})

require("nvim-treesitter.configs").setup({
  ensure_installed = "maintained",
  highlight = {
    enable = true,
  },
  indent = {
    enable = true,
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        ["]m"] = "@function.outer",
        ["]]"] = "@class.outer",
      },
      goto_next_end = {
        ["]M"] = "@function.outer",
        ["]["] = "@class.outer",
      },
      goto_previous_start = {
        ["[m"] = "@function.outer",
        ["[["] = "@class.outer",
      },
      goto_previous_end = {
        ["[M"] = "@function.outer",
        ["[]"] = "@class.outer",
      },
    },
  },
  autotag = { enable = true },
})

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(_, bufnr)
  local function buf_set_keymap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end
  local function buf_set_option(...)
    vim.api.nvim_buf_set_option(bufnr, ...)
  end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

  -- Mappings.
  local opts = { noremap = true, silent = true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  buf_set_keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
  buf_set_keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  buf_set_keymap("n", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  buf_set_keymap("n", "<space>wa", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)
  buf_set_keymap("n", "<space>wr", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)
  buf_set_keymap("n", "<space>wl", "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", opts)
  buf_set_keymap("n", "<space>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
  buf_set_keymap("n", "<space>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  buf_set_keymap("n", "<space>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
  buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
  buf_set_keymap("n", "<space>e", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
  buf_set_keymap("n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", opts)
  buf_set_keymap("n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", opts)
  buf_set_keymap("n", "<space>q", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", opts)
  buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)

  -- trouble.nvim
  buf_set_keymap("n", "<space>xx", "<cmd>Trouble<CR>", opts)
  buf_set_keymap("n", "<space>xw", "<cmd>Trouble lsp_workspace_diagnostics<CR>", opts)
  buf_set_keymap("n", "<space>xd", "<cmd>Trouble lsp_document_diagnostics<CR>", opts)
  buf_set_keymap("n", "<space>xl", "<cmd>Trouble loclist<CR>", opts)
  buf_set_keymap("n", "<space>xq", "<cmd>Trouble quickfix<CR>", opts)
  buf_set_keymap("n", "gR", "<cmd>Trouble lsp_references<CR>", opts)
end

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
  opts.on_attach = on_attach

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
