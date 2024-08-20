local hm = require("heime")

return hm.task({
  name = "rust",
  description = "setup and configure rust",
  run = function(ctx)
    ctx:copy_template("config.toml", hm.path(hm.home_dir, ".cargo", "config.toml"))
  end,
})
