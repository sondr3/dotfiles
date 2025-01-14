local hm = require("heime")

return hm.task({
  name = "setup",
  run = function(ctx)
    ctx:copy("config/heime/config.lua", hm.path(hm.config_dir, "heime", "config.lua"))
  end,
})
