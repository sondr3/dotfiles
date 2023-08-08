return {
  "nvim-lua/popup.nvim",
  "nvim-lua/plenary.nvim",
  "kkharji/sqlite.lua",

  -- Personal plugins
  {
    url = "git@github.com:sondr3/empyreum.git",
    -- config = function()
    --   require("empyreum").colorscheme({ variant = "light" })
    -- end,
  },
  -- dotfile manager
  { url = "git@github.com:sondr3/heime.git" },

  -- Theme
  {
    "andersevenrud/nordic.nvim",
    lazy = false,
    config = function()
      require("nordic").colorscheme({})
    end,
  },

  {
    "nvim-tree/nvim-web-devicons",
    config = function()
      require("nvim-web-devicons").setup({ default = true })
    end,
  },

  { "stevearc/dressing.nvim", event = "VeryLazy" },
  { "folke/which-key.nvim" },

  -- Rust sweetness
  { "simrat39/rust-tools.nvim" },

  -- Haskell
  { "ndmitchell/ghcid", rtp = "plugins/nvim" },
  {
    "MrcJkb/haskell-tools.nvim",
    branch = "2.x.x",
    dependencies = {
      "neovim/nvim-lspconfig",
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
    },
    init = function()
      local lsp = require("config.plugins.lsp")
      lsp.fidget()
      lsp.signature()
      lsp.null_ls()
      vim.g.haskell_tools = {

        hls = {
          on_attach = function(client, bufnr, ht)
            local opts = { noremap = true, silent = true }
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
            lsp.on_attach(client, bufnr)
          end,
          capabilities = lsp.capabilities(),
          default_settings = {
            haskell = {
              formattingProvider = "ormolu",
              checkProject = true,
            },
          },
        },
      }
    end,
  },

  -- Dhall
  { "vmchale/dhall-vim" },

  -- PureScript
  { "purescript-contrib/purescript-vim" },

  -- Jinja
  { "Glench/Vim-Jinja2-Syntax", ft = "jinja" },

  -- Djot
  { "jgm/djot", ft = "dj", build = "mv editors/vim ." },
}
