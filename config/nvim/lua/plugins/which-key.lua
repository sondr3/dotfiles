vim.pack.add({ "https://github.com/folke/which-key.nvim" })

local wk = require("which-key")

wk.setup({})

wk.add({
	{ "<leader>f", group = "file" },
})
