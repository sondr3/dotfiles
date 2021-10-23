local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua | PackerCompile
  augroup end
]])

require("packer").startup({
  function(use)
    -- Package manager
    use 'wbthomason/packer.nvim'
 
    -- Best thing ever
    use {
      'nvim-telescope/telescope.nvim',
      requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
    }
 
    -- Theme
    use 'shaunsingh/nord.nvim'
 
    -- tree-sitter for highlighting goodness
    use {
      "nvim-treesitter/nvim-treesitter",
      branch = "0.5-compat",
      run = ":TSUpdate"
    }
 
    -- LSP configuration
    use "neovim/nvim-lspconfig"
    use "hrsh7th/nvim-compe"
    use "simrat39/rust-tools.nvim"
  
    if packer_bootstrap then
      require('packer').sync()
    end
  end,
  config = {
    display = {
      open_fn = require("packer.util").float,
    }
  }
})
