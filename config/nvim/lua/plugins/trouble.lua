vim.pack.add({ "https://github.com/folke/trouble.nvim" })

require("trouble").setup({})

local config = require("fzf-lua.config")
local actions = require("trouble.sources.fzf").actions

config.defaults.actions.files["ctrl-t"] = actions.open

vim.keymap.set(
	"n",
	"<leader>xx",
	function() vim.cmd([[Trouble diagnostics toggle]]) end,
	{ desc = "Diagnostics" }
)

vim.keymap.set(
	"n",
	"<leader>xb",
	function() vim.cmd([[Trouble diagnostics toggle filter.buf=0]]) end,
	{ desc = "Buffer Diagnostics" }
)

vim.keymap.set(
	"n",
	"<leader>xq",
	function() vim.cmd([[Trouble qflist toggle]]) end,
	{ desc = "Quickfix" }
)
