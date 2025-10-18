vim.pack.add({ "https://github.com/nvim-mini/mini.nvim" })

require("mini.extra").setup()
require("mini.icons").setup({})

-- around/inside modifiers for textobjects
require("mini.ai").setup({ n_lines = 500 })

-- modifiers for surrounding things
require("mini.surround").setup()

require("mini.statusline").setup({
	use_icons = true,
})

require("mini.tabline").setup({
	use_icons = true,
})

require("mini.pairs").setup()
require("mini.indentscope").setup()
