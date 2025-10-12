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

local win_config = function()
	local height = math.floor(0.618 * vim.o.lines)
	local width = math.floor(0.618 * vim.o.columns)
	return {
		anchor = "NW",
		height = height,
		width = width,
		row = math.floor(0.5 * (vim.o.lines - height)),
		col = math.floor(0.5 * (vim.o.columns - width)),
	}
end

local pick = require("mini.pick")
pick.setup({
	options = {
		use_cache = true,
	},
	window = { config = win_config() },
})

vim.keymap.set("n", "<leader>ff", pick.builtin.files, { desc = "[f]ind [f]iles" })
vim.keymap.set("n", "<leader>,", pick.builtin.buffers, { desc = "Buffers" })
vim.keymap.set("n", "<leader>/", pick.builtin.grep_live, { desc = "Gutters" })
