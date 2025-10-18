vim.pack.add({ "https://github.com/dmtrKovalenko/fff.nvim" })

vim.api.nvim_create_autocmd("PackChanged", {
	callback = function(event)
		if
			event.data.spec and event.data.spec.name == "fff.nvim" and event.data.kind == "install"
			or event.data.kind == "update"
		then
			require("fff.download").download_or_build_binary()
		end
	end,
})

vim.g.fff = {
	lazy_sync = true,
}

vim.keymap.set(
	"n",
	"<leader>ff",
	function() require("fff").find_files() end,
	{ desc = "FFFind files" }
)
