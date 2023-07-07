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
    dependencies = {
      "neovim/nvim-lspconfig",
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
    },
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
