vim.pack.add({
	"https://github.com/yorickpeterse/nvim-grey",
})

vim.cmd.colorscheme("grey")

require("options")
require("autocmds")
require("keymaps")
require("plugins")
