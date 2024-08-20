local hm = require("heime")

return hm.task({
  name = "fish",
  description = "setup and configure fish",
  run = function(ctx)
    ctx:copy_file("config.fish", hm.path(hm.config_dir, "fish", "config.fish"))
  end,
})
