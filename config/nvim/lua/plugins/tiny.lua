vim.pack.add({
	"https://github.com/rachartier/tiny-cmdline.nvim",
	"https://github.com/rachartier/tiny-inline-diagnostic.nvim",
	"https://github.com/rachartier/tiny-code-action.nvim",
})

require("tiny-inline-diagnostic").setup({
	preset = "modern",
	options = {
		show_source = { if_many = true },
		transparent_bg = true,
		use_icons_from_diagnostic = true,
	},
})

require("tiny-cmdline").setup({
	on_reposition = require("tiny-cmdline").adapters.blink,
})

require("tiny-code-action").setup({
	backend = "vim",
	picker = {
		"buffer",
		opts = {
			hotkeys = true,
			auto_preview = true,
		},
	},
})
