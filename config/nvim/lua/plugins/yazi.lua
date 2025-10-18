vim.pack.add({
	"https://github.com/mikavilpas/yazi.nvim",
	"https://github.com/nvim-lua/plenary.nvim",
})

local yazi = require("yazi")
yazi.setup({
	open_for_directories = true,
	keymaps = {
		show_help = "<f1>",
	},
})

vim.keymap.set("n", "<leader>-", yazi.yazi, { desc = "Yazi" })
vim.keymap.set("n", "<leader>cw", yazi.toggle, { desc = "Toggle yazi" })

-- override netrw
vim.api.nvim_create_autocmd("UIEnter", {
	callback = function() yazi.setup({ open_for_directories = true }) end,
})
