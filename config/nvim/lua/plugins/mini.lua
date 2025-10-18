vim.pack.add({ "https://github.com/nvim-mini/mini.nvim" })

-- around/inside modifiers for textobjects
require("mini.ai").setup({ n_lines = 500 })

require("mini.statusline").setup({
	use_icons = true,
})
require("mini.pairs").setup()
require("mini.indentscope").setup()
