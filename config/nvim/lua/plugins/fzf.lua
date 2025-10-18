vim.pack.add({
	"https://github.com/ibhagwan/fzf-lua",
	"https://github.com/nvim-tree/nvim-web-devicons",
})

local fzf = require("fzf-lua")
fzf.setup({
	"default",
	globals = {
		file_icons = "devicons",
	},
})
fzf.register_ui_select()

vim.keymap.set("n", "<leader>,", fzf.buffers, { desc = "Buffers" })
vim.keymap.set("n", "<leader>/", fzf.live_grep, { desc = "Gutters" })
