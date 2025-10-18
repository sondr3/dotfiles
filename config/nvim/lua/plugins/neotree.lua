vim.pack.add({
	{
		src = "https://github.com/nvim-neo-tree/neo-tree.nvim",
		version = vim.version.range("3"),
	},
	"https://github.com/nvim-lua/plenary.nvim",
	"https://github.com/MunifTanjim/nui.nvim",
	"https://github.com/nvim-tree/nvim-web-devicons",
})

local neotree = require("neo-tree")
neotree.setup({
	filesystem = {
		hijack_netrw_behavior = "disabled",
	},
})

vim.keymap.set("n", "<leader>ft", function() vim.cmd("Neotree") end, { desc = "[f]ile [t]ree" })
