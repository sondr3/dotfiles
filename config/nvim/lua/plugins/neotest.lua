vim.pack.add({
	"https://github.com/nvim-lua/plenary.nvim",
	"https://github.com/antoinemadec/FixCursorHold.nvim",
	"https://github.com/nvim-treesitter/nvim-treesitter",
	"https://github.com/nvim-neotest/nvim-nio",
	"https://github.com/nvim-neotest/neotest",
	"https://github.com/mrcjkb/rustaceanvim",
})

local neotest = require("neotest")
neotest.setup({
	adapters = {
		require("rustaceanvim.neotest"),
	},
})

vim.keymap.set("n", "<leader>ta", neotest.run.attach, { desc = "Attach to Test" })
vim.keymap.set("n", "<leader>tp", neotest.output_panel.toggle, { desc = "Toggle panel" })
vim.keymap.set("n", "<leader>tc", neotest.output_panel.close, { desc = "Close panel" })
vim.keymap.set(
	"n",
	"<leader>tt",
	function() neotest.run.run(vim.fn.expand("%")) end,
	{ desc = "Run File" }
)
vim.keymap.set(
	"n",
	"<leader>tT",
	function() neotest.run.run(vim.uv.cwd()) end,
	{ desc = "Run All Test Files" }
)
vim.keymap.set("n", "<leader>tr", neotest.run.run, { desc = "Run Nearest" })
vim.keymap.set("n", "<leader>tl", neotest.run.run_last, { desc = "Run Last" })
vim.keymap.set("n", "<leader>ts", neotest.summary.toggle, { desc = "Toggle Summary" })
vim.keymap.set(
	"n",
	"<leader>to",
	function() neotest.output.open({ enter = true, auto_close = true }) end,
	{ desc = "Show Output" }
)
vim.keymap.set("n", "<leader>tS", neotest.run.stop, { desc = "Stop" })
vim.keymap.set(
	"n",
	"<leader>tw",
	function() neotest.watch.toggle(vim.fn.expand("%")) end,
	{ desc = "Toggle Watch" }
)
