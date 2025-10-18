vim.pack.add({ "https://github.com/shellRaining/hlchunk.nvim" })

require("hlchunk").setup({
	chunk = {
		enable = true,
		chars = {
			right_arrow = "⊸",
		},
	},
	line_num = {
		enable = true,
		use_treesitter = true,
	},
})
