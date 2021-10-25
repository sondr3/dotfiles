local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  })
end

vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])

require("packer").startup({
  function(use)
    -- Package manager
    use("wbthomason/packer.nvim")

    -- For the impatient, used to cache compiled
    use("lewis6991/impatient.nvim")

    -- Best thing ever
    use({
      "nvim-telescope/telescope.nvim",
      requires = { { "nvim-lua/popup.nvim" }, { "nvim-lua/plenary.nvim" } },
    })
    use({
      "nvim-telescope/telescope-fzf-native.nvim",
      run = "make",
    })

    -- Theme
    use("maaslalani/nordbuddy")

    -- Status line
    use({
      "famiu/feline.nvim",
      requires = { "kyazdani42/nvim-web-devicons" },
    })

    -- Git gutter signs
    use({
      "lewis6991/gitsigns.nvim",
      requires = { "nvim-lua/plenary.nvim" },
    })

    use("akinsho/toggleterm.nvim")

    -- Show indent lines
    use("lukas-reineke/indent-blankline.nvim")

    -- tree-sitter for highlighting goodness
    use({
      "nvim-treesitter/nvim-treesitter",
      branch = "0.5-compat",
      run = ":TSUpdate",
    })
    use("nvim-treesitter/nvim-treesitter-textobjects")

    -- LSP configuration
    use("neovim/nvim-lspconfig")
    use("williamboman/nvim-lsp-installer")
    use("hrsh7th/cmp-nvim-lsp")
    use("onsails/lspkind-nvim")

    -- Snippets
    use({
      "L3MON4D3/LuaSnip",
      requires = { "saadparwaiz1/cmp_luasnip" },
    })

    -- Autocompletion
    use({
      "hrsh7th/nvim-cmp",
      requires = {
        {
          "neovim/nvim-lspconfig",
          "hrsh7th/cmp-nvim-lsp",
          "hrsh7th/cmp-buffer",
          "hrsh7th/cmp-path",
          "hrsh7th/cmp-nvim-lua",
          "saadparwaiz1/cmp_luasnip",
        },
      },
    })

    -- Rust sweetness
    use("simrat39/rust-tools.nvim")

    if packer_bootstrap then
      require("packer").sync()
    end
  end,
  config = {
    display = {
      open_fn = require("packer.util").float,
    },
    compile_path = vim.fn.stdpath("config") .. "/lua/packer_compiled.lua",
  },
})
