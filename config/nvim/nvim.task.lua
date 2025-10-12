local hm = require("heime")

return hm.task({
	name = "neovim",
	description = "setup and configure neovim",
	run = function(ctx)
		ctx:copy(".", hm.path(hm.config_dir, "nvim"))
		ctx:copy("stylua.toml", hm.path(hm.config_dir, "nvim", "stylua.toml"))
		ctx:copy("nvim-pack-lock.json", hm.path(hm.config_dir, "nvim", "nvim-pack-lock.json"))
	end,
})
