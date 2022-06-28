-- Use Neovim file type detection
vim.g.do_filetype_lua = 1
vim.g.did_load_filetypes = 0

local function ensure(owner, repo)
  local install_path = string.format("%s/packer/start/%s", vim.fn.stdpath("data") .. "/site/pack", repo, repo)

  if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
    vim.fn.system({
      "git",
      "clone",
      "--depth",
      "1",
      string.format("https://github.com/%s/%s", owner, repo),
      install_path,
    })
    vim.api.nvim_command("packadd " .. repo)

    -- Bootstrap packages if we installed packer.nvim
    if repo == "packer.nvim" then
      require("packer").sync()
    end
  end
end

-- Bootstrap stuff
ensure("wbthomason", "packer.nvim")
ensure("lewis6991", "impatient.nvim")

-- Speed up loading Nvim files
require("impatient")
if not pcall(require, "packer_compiled") then
  require("packer").compile()
end

-- Setup Fennel
ensure("rktjmp", "hotpot.nvim")
local ok, hotpot = pcall(require, "hotpot")
if ok then
  hotpot.setup({ provide_require_fennel = true })
else
  print("Missing 'hotpot.nvim'")
end

require("plugins")
require("settings")

require("statusline")
