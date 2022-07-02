local hm = require("heime")

return hm.task({
  name = "neovim",
  description = "setup and configure neovim",
  run = function(ctx)
    ctx:copy_directory("nvim", ctx:config_file("nvim"))
    ctx:copy_file("stylua.toml", ctx:config_file("nvim", "stylua.toml"))
    ctx:write_string(ctx:config_file("nvim", ".styluaignore"), "lua/packer_compiled.lua")
  end,
})
