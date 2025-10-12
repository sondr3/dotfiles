vim.pack.add({
	"https://github.com/andersevenrud/nordic.nvim",
	"https://github.com/lewis6991/gitsigns.nvim",
})
vim.cmd.colorscheme("nordic")

require("options")
require("keymaps")
require("plugins")
