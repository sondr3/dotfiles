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

local group = vim.api.nvim_create_augroup("packer_user_config", { clear = true })
vim.api.nvim_create_autocmd("BufWritePost", {
  group = group,
  pattern = "plugins.lua",
  callback = function(args)
    vim.api.nvim_command("source " .. args.file .. " | PackerCompile")
  end,
})

require("packer").startup({
  function(use)
    -- Package manager
    use("wbthomason/packer.nvim")
    -- For the impatient, used to cache compiled
    use("lewis6991/impatient.nvim")

    -- Personal plugins
    -- Theme
    use({
      "sondr3/empyreum",
      -- config = function()
      --   require("empyreum").colorscheme({ variant = "light" })
      -- end,
    })

    -- dotfile manager
    use("sondr3/heime")

    -- Best thing ever
    use({
      "nvim-telescope/telescope.nvim",
      requires = {
        "nvim-lua/popup.nvim",
        "nvim-lua/plenary.nvim",
        "nvim-telescope/telescope-file-browser.nvim",
      },
      config = function()
        require("config/telescope")
      end,
    })
    use({
      "nvim-telescope/telescope-fzf-native.nvim",
      run = "make",
    })
    use({
      "nvim-telescope/telescope-frecency.nvim",
      requires = { "tami5/sqlite.lua" },
      config = function()
        require("telescope").load_extension("frecency")
      end,
    })

    use({
      "stevearc/dressing.nvim",
      config = function()
        require("dressing").setup({})
      end,
    })

    -- Move around fast
    use({
      "ggandor/lightspeed.nvim",
      config = function()
        require("lightspeed").setup({})
      end,
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

    -- Magit in Neovim?
    use({
      "TimUntersberger/neogit",
      requires = {
        "nvim-lua/plenary.nvim",
        "sindrets/diffview.nvim",
      },
      config = function()
        require("neogit").setup({
          disable_commit_confirmation = true,
          disable_insert_on_commit = false,
          integrations = {
            diffview = true,
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
        function _G.set_terminal_keymaps()
          local opts = { noremap = true }
          vim.api.nvim_buf_set_keymap(0, "t", "<esc>", [[<C-\><C-n>]], opts)
          vim.api.nvim_buf_set_keymap(0, "t", "<C-w>h", [[<C-\><C-n><C-W>h]], opts)
          vim.api.nvim_buf_set_keymap(0, "t", "<C-w>j", [[<C-\><C-n><C-W>j]], opts)
          vim.api.nvim_buf_set_keymap(0, "t", "<C-w>k", [[<C-\><C-n><C-W>k]], opts)
          vim.api.nvim_buf_set_keymap(0, "t", "<C-w>l", [[<C-\><C-n><C-W>l]], opts)
        end

        vim.cmd([[ autocmd! TermOpen term://* lua set_terminal_keymaps() ]])
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
        require("config/which-key")
      end,
    })

    -- tree-sitter for highlighting goodness
    use({
      "nvim-treesitter/nvim-treesitter",
      run = ":TSUpdate",
      config = function()
        require("config/tree-sitter")
      end,
    })
    use("nvim-treesitter/nvim-treesitter-textobjects")
    use("windwp/nvim-ts-autotag")
    use("JoosepAlviste/nvim-ts-context-commentstring")

    -- LSP configuration
    use({
      "neovim/nvim-lspconfig",
      config = function()
        require("config/lsp")
      end,
    })

    use({
      "williamboman/mason.nvim",
      requires = {
        "williamboman/mason-lspconfig.nvim",
        "WhoIsSethDaniel/mason-tool-installer.nvim",
      },
    })

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

    -- Show where stuff is failing in projects
    use({
      "folke/trouble.nvim",
      requires = "kyazdani42/nvim-web-devicons",
      config = function()
        require("trouble").setup({})
      end,
    })

    -- Make non-LSP great again
    use({
      "jose-elias-alvarez/null-ls.nvim",
      requires = { "nvim-lua/plenary.nvim" },
    })
    use({
      "lukas-reineke/lsp-format.nvim",
      config = function()
        require("lsp-format").setup({ sync = true })
      end,
    })

    -- LSP loading spinners and information
    use({
      "j-hui/fidget.nvim",
      config = function()
        require("fidget").setup({})
      end,
    })

    -- Snippets
    use({
      "L3MON4D3/LuaSnip",
      requires = { "saadparwaiz1/cmp_luasnip", "rafamadriz/friendly-snippets" },
      config = function()
        require("luasnip").config.setup({
          history = false,
          updateevents = "TextChanged,TextChangedI",
          enable_autosnippets = true,
        })
        require("luasnip.loaders.from_vscode").lazy_load()
      end,
    })

    -- Autocompletion
    use({
      "hrsh7th/nvim-cmp",
      requires = {
        "neovim/nvim-lspconfig",
        "hrsh7th/cmp-nvim-lsp",
        "hrsh7th/cmp-nvim-lua",
        "hrsh7th/cmp-buffer",
        "hrsh7th/cmp-path",
        "hrsh7th/cmp-cmdline",
        "saadparwaiz1/cmp_luasnip",
      },
      config = function()
        require("config/completion")
      end,
    })

    -- Tab magic to jump out of stuff
    use({
      "abecodes/tabout.nvim",
      config = function()
        require("tabout").setup({})
      end,
      wants = { "nvim-treesitter" },
      after = { "nvim-cmp" },
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

    -- Commenting
    use({
      "numToStr/Comment.nvim",
      requires = { "JoosepAlviste/nvim-ts-context-commentstring" },
      config = function()
        require("config/comments")
      end,
    })

    -- Surrounding stuff
    use({
      "kylechui/nvim-surround",
      config = function()
        require("nvim-surround").setup({})
      end,
    })

    -- Rust sweetness
    use("simrat39/rust-tools.nvim")

    -- LaTeX
    use({
      "lervag/vimtex",
      config = function()
        vim.g.vimtex_view_method = "zathura"
        vim.g.vimtex_view_use_temp_files = true
        vim.g.vimtex_compiler_method = "latexmk"
        vim.g.vimtex_quickfix_ignore_filters = {
          "Overfull",
          "Underfull",
          "LaTeX hooks Warning",
          "Package unicode-math Warning",
        }
      end,
    })

    -- Haskell
    use({
      "ndmitchell/ghcid",
      rtp = "plugins/nvim",
    })

    -- Dhall
    use({ "vmchale/dhall-vim" })

    -- PureScript
    use({ "purescript-contrib/purescript-vim" })

    if PACKER_BOOTSTRAP then
      require("packer").sync()
    end
  end,
  config = {
    ensure_dependencies = true,
    display = {
      open_fn = require("packer.util").float,
    },
    git = {
      default_url_format = "git@github.com:%s",
    },
    compile_path = vim.fn.stdpath("config") .. "/lua/packer_compiled.lua",
  },
})
