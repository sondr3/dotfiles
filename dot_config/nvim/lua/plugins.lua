local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP = fn.system({
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
      config = [[ require("config/telescope") ]],
    })
    use({
      "nvim-telescope/telescope-fzf-native.nvim",
      run = "make",
    })

    -- Theme
    use({
      "andersevenrud/nordic.nvim",
      config = function()
        require("nordic").colorscheme({})
      end,
    })

    -- Status line
    use({
      "famiu/feline.nvim",
      requires = { "kyazdani42/nvim-web-devicons" },
    })

    -- Git gutter signs
    use({
      "lewis6991/gitsigns.nvim",
      requires = { "nvim-lua/plenary.nvim" },
      config = function()
        require("gitsigns").setup({
          signs = {
            add = { hl = "GitGutterAdd", text = "+" },
            change = { hl = "GitGutterChange", text = "~" },
            delete = { hl = "GitGutterDelete", text = "_" },
            topdelete = { hl = "GitGutterDelete", text = "‾" },
            changedelete = { hl = "GitGutterChange", text = "~" },
          },
        })
      end,
    })

    -- Have the terminal floating about
    use({
      "akinsho/toggleterm.nvim",
      config = function()
        require("toggleterm").setup({
          open_mapping = [[<c-t>]],
          size = 30,
        })
      end,
    })

    -- Automatic brackets
    use({
      "windwp/nvim-autopairs",
      config = function()
        require("nvim-autopairs").setup({
          check_ts = true,
        })
      end,
    })

    -- Show indent lines
    use({
      "lukas-reineke/indent-blankline.nvim",
      config = function()
        require("indent_blankline").setup({
          buftype_exclude = { "terminal", "nofile" },
          filetype_exclude = { "help", "packer" },
          use_treesitter = true,
          show_trailing_blankline_indent = false,
        })
      end,
    })

    -- which-key for keybinding popup
    use({
      "folke/which-key.nvim",
      config = function()
        require("which-key").setup({})
      end,
    })

    -- tree-sitter for highlighting goodness
    use({
      "nvim-treesitter/nvim-treesitter",
      run = ":TSUpdate",
      config = [[ require("config/tree-sitter") ]],
    })
    use("nvim-treesitter/nvim-treesitter-textobjects")
    use("windwp/nvim-ts-autotag")

    -- LSP configuration
    use("neovim/nvim-lspconfig")
    use("hrsh7th/cmp-nvim-lsp")
    use("onsails/lspkind-nvim")
    use({
      "williamboman/nvim-lsp-installer",
      config = [[ require("config/lsp/lsp-installer") ]],
    })
    use({
      "folke/trouble.nvim",
      requires = "kyazdani42/nvim-web-devicons",
      config = function()
        require("trouble").setup({})
      end,
    })

    use({
      "jose-elias-alvarez/null-ls.nvim",
      config = function()
        require("null-ls").config({
          sources = { require("null-ls").builtins.formatting.rustfmt },
        })
        require("lspconfig")["null-ls"].setup({})
      end,
      requires = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    })

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
      config = [[ require("config/completion") ]],
    })

    -- Rust sweetness
    use("simrat39/rust-tools.nvim")

    if PACKER_BOOTSTRAP then
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
