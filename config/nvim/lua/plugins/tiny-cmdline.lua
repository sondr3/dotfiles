vim.pack.add({ "https://github.com/rachartier/tiny-cmdline.nvim" })

require("tiny-cmdline").setup({
	on_reposition = require("tiny-cmdline").adapters.blink,
})
