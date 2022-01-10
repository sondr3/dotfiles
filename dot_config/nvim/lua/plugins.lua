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

    -- Personal Theme
    use({
      "/home/sondre/Code/ts/a-theme/",
      -- config = function()
      --   require("modus").colorscheme({ theme = "modus-operandi" })
      -- end,
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
          direction = "float",
          shade_terminals = false,
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
    use("JoosepAlviste/nvim-ts-context-commentstring")

    -- LSP configuration
    use("neovim/nvim-lspconfig")
    use("hrsh7th/cmp-nvim-lsp")
    use("onsails/lspkind-nvim")
    use({
      "ray-x/lsp_signature.nvim",
      config = function()
        require("lsp_signature").setup({
          doc_lines = 0,
          floating_window = false,
          hint_prefix = " ",
        })
      end,
    })
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
      -- "/home/sondre/Code/lua/null-ls.nvim",
      "jose-elias-alvarez/null-ls.nvim",
      config = function()
        require("null-ls").setup({
          sources = {
            require("null-ls").builtins.formatting.stylua,
            require("null-ls").builtins.formatting.prettierd,
            require("null-ls").builtins.formatting.stylelint,
            require("null-ls").builtins.diagnostics.eslint_d.with({
              extra_args = { "--cache" },
            }),
          },
          on_attach = function(client)
            if client.resolved_capabilities.document_formatting then
              vim.cmd("autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()")
            end
          end,
        })
      end,
      requires = { "nvim-lua/plenary.nvim" },
    })

    -- Snippets
    use({
      "L3MON4D3/LuaSnip",
      requires = { "saadparwaiz1/cmp_luasnip", "rafamadriz/friendly-snippets" },
      config = function()
        require("luasnip").config.setup({
          -- disable jumping back into snippets once left
          history = false,
        })
        require("luasnip.loaders.from_vscode").lazy_load()
      end,
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
          "saadparwaiz1/cmp_luasnip",
        },
      },
      config = [[ require("config/completion") ]],
    })

    -- Commenting
    use({
      "numToStr/Comment.nvim",
      requires = { "JoosepAlviste/nvim-ts-context-commentstring" },
      config = [[ require("config/comments") ]],
    })

    -- Surrounding stuff
    use({
      "blackCauldron7/surround.nvim",
      config = function()
        require("surround").setup({ mappings_style = "surround" })
      end,
    })

    -- Rust sweetness
    use("simrat39/rust-tools.nvim")

    if PACKER_BOOTSTRAP then
      require("packer").sync()
    end
  end,
  config = {
    ensure_dependencies = true,
    display = {
      open_fn = require("packer.util").float,
    },
    compile_path = vim.fn.stdpath("config") .. "/lua/packer_compiled.lua",
  },
})
