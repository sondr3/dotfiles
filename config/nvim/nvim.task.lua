local hm = require("heime")

return hm.task({
  name = "neovim",
  description = "setup and configure neovim",
  run = function(ctx)
    ctx:copy_directory("nvim", hm.path(hm.config_dir, "nvim"))
    ctx:copy_file("stylua.toml", hm.path(hm.config_dir, "nvim", "stylua.toml"))
    ctx:copy_file("lazy-lock.json", hm.path(hm.config_dir, "nvim", "lazy-lock.json"))
  end,
})
